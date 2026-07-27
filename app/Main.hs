module Main (main) where

import qualified Cli
import qualified Api
import qualified Config
import qualified History
import qualified PendingReviews
import qualified Tui
import Util (shortErr, strPadLeft, strPadRight, trySync)

import Control.Applicative ((<|>))
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (SomeException, bracket_, try)
import System.Environment (lookupEnv)
import System.Exit (die)

import Data.Time (getCurrentTime, getCurrentTimeZone, utcToLocalTime, TimeZone, UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.List (nub, partition, sortOn)
import Data.Maybe (fromMaybe)
import Data.Map.Strict qualified as M
import qualified Data.Set as Set
import qualified Data.Text as T

-- | TEMPORARY debug tracing: writes to a file instead of stdout because
-- these calls happen inside the Brick alternate-screen session, where
-- stdout writes get overwritten by Vty's own redraws and never show up in
-- the terminal scrollback.
debugLog :: String -> IO ()
debugLog msg = appendFile "/tmp/kroki_debug.log" (msg <> "\n")

main :: IO ()
main = do
  opts <- Cli.parseCli
  cfg  <- Config.loadConfig

  envToken <- lookupEnv "WANIKANI_API_TOKEN"

  let token =
        Cli.optToken opts
        <|> envToken
        <|> Config.cfgToken cfg

  let requireToken =
        maybe
          (die "Missing API token. Provide --token, set WANIKANI_API_TOKEN, or put token=... into ~/.config/kroki/config")
          pure
          token

  let runLeechList t history = do
        tz <- getCurrentTimeZone
        let entries = sortOn (negate . leechTotal) (M.elems (History.activeLeeches history))
        subjects <- Api.getSubjectsByIds t (map History.leSubjectId entries)
        let subjMap = M.fromList [ (Api.subjId s, s) | s <- subjects ]

        putStrLn "Item                        Lvl  Meaning  Reading  Last seen"
        putStrLn "--------------------------------------------------------------"
        mapM_ (putStrLn . fmtLeechRow tz subjMap) entries

      -- | Practice tracked leeches, batch-size at a time (worst first).
      -- Unlike 'study', the queue comes straight from leeches.json (not
      -- WaniKani's "available reviews"), and results are never submitted to
      -- WaniKani -- 'submitFn' here just records the round locally so a
      -- clean answer can let a leech drop off the list (see
      -- History.applyPracticeSession).
      runLeechStudy t initialHistory = do
        user <- Api.getUser t
        summary0 <- Api.getSummary t
        userSynonyms <- Api.getMeaningSynonyms t
        let batchSize =
              Config.cfgBatchSize cfg <|> Just Config.defaultBatchSize
            rqAfter =
              fromMaybe Config.defaultRequeueAfter (Config.cfgRequeueAfter cfg)
            audioAutoplay = fromMaybe False (Config.cfgAudioAutoplay cfg)
            audioPlayer   = Config.cfgAudioPlayer cfg
            raw = fromMaybe 10 batchSize
            n   = if raw == 0 then maxBound else raw
            orderedIds = map History.leSubjectId
                           (sortOn (negate . leechTotal)
                             (M.elems (History.activeLeeches initialHistory)))

            loop history remainingIds
              | null remainingIds = pure ()
              | otherwise = do
                  let (batchIds, restIds) = splitAt n remainingIds
                  now <- getCurrentTime
                  tz  <- getCurrentTimeZone

                  leechSubjects <- applyStudyMaterials userSynonyms
                                     <$> Api.getSubjectsByIds t batchIds
                  allSubjMap <- fetchSubjectContext t userSynonyms leechSubjects
                  asgs <- Api.getAssignmentsBySubjectIds t batchIds
                  let subjToAsg = M.fromList [ (Api.asSubjectId a, a) | a <- asgs ]
                      asgToInfo = M.fromList
                        [ (Api.asId asg, (subj, asg))
                        | subj <- leechSubjects
                        , Just asg <- [M.lookup (Api.subjId subj) subjToAsg]
                        ]
                      priorWrong = History.historyCounts history
                      refreshSummary = do
                        now'     <- getCurrentTime
                        summary' <- Api.getSummary t
                        pure (now', summary')
                      submitPractice subs =
                        pure Tui.SubmitResult
                          { Tui.srMessage = "Practice recorded locally (not submitted to WaniKani)."
                          , Tui.srHasMore = not (null restIds)
                          , Tui.srDetails = map (fmtLeechPractice asgToInfo) subs
                          }

                  (wantsMore, sessionCounts) <-
                    Tui.runStudyTui
                      Tui.StudyConfig
                        { Tui.scRequeueAfter  = rqAfter
                        , Tui.scAudioPlayer   = audioPlayer
                        , Tui.scAudioAutoplay = audioAutoplay
                        , Tui.scPracticeOnly  = True
                        }
                      user summary0 now tz allSubjMap subjToAsg priorWrong leechSubjects
                      refreshSummary submitPractice (submitSynonymWith t)
                  now2 <- getCurrentTime
                  -- 'Nothing' means the practice round was abandoned before
                  -- finishing; leave the leech history untouched rather than
                  -- retiring subjects that were never fully answered.
                  let history' = case sessionCounts of
                        Nothing -> history
                        Just cs -> History.applyPracticeSession now2 batchIds cs history
                  History.saveHistory history'
                  if wantsMore && not (null restIds)
                    then loop history' restIds
                    else pure ()

        loop initialHistory orderedIds

      runStudy studyOpts = do
        t <- requireToken
        let batchSize =
              Cli.studyBatchSize studyOpts
              <|> Config.cfgBatchSize cfg
              <|> Just Config.defaultBatchSize
            rqAfter =
              fromMaybe Config.defaultRequeueAfter
                (Cli.studyRequeueAfter studyOpts <|> Config.cfgRequeueAfter cfg)
            audioAutoplay = fromMaybe False (Config.cfgAudioAutoplay cfg)
            raw = fromMaybe 10 batchSize
            n   = if raw == 0 then maxBound else raw
        now  <- getCurrentTime
        tz   <- getCurrentTimeZone
        user <- Api.getUser t
        summary <- Api.getSummary t
        userSynonyms <- Api.getMeaningSynonyms t

        -- A create-review POST can throw on the client side (timeout,
        -- connection closed) even though WaniKani already processed it --
        -- the response just never made it back. Resubmitting that one is
        -- pointless: the assignment is no longer "available" once a review
        -- is recorded for it, so WaniKani will keep rejecting the retry
        -- forever. Split out any "failure" that's actually already recorded
        -- on WaniKani's side so it gets dropped instead of stuck retrying.
        let dropAlreadyRecorded failed
              | null failed = pure (failed, 0 :: Int)
              | otherwise = do
                  now' <- getCurrentTime
                  stillAvailable <- Api.getStillAvailableAssignmentIds t now'
                    (map PendingReviews.prAssignmentId failed)
                  let (genuine, alreadyRecorded) =
                        partition
                          (\p -> PendingReviews.prAssignmentId p `Set.member` stillAvailable)
                          failed
                  pure (genuine, length alreadyRecorded)

            -- Resubmit anything left over from a previous run's network
            -- trouble, using each item's original 'prCreatedAt' (the review
            -- really happened then, not now). Returns the assignment ids
            -- that are still failing after this attempt, so this run's
            -- fresh batch doesn't ask about them again.
            retryPendingReviews = do
              pending <- PendingReviews.loadPendingReviews
              if null pending
                then pure Set.empty
                else do
                  (toRetry, droppedN) <- dropAlreadyRecorded pending
                  outcomes <- mapConcurrentlyN maxParallelSubmits postPendingReview toRetry
                  let stillFailing = [ p | (p, Left _) <- outcomes ]
                      succeededN   = length outcomes - length stillFailing
                  PendingReviews.savePendingReviews stillFailing
                  putStrLn $
                    "Retrying " <> show (length toRetry)
                    <> " review(s) that failed to reach WaniKani last time: "
                    <> show succeededN <> " succeeded"
                    <> ( if droppedN == 0
                           then ""
                           else ", " <> show droppedN
                             <> " were already recorded on WaniKani (dropped)"
                       )
                    <> ( if null stillFailing
                           then "."
                           else ", " <> show (length stillFailing)
                             <> " still failing (will retry again next run)."
                       )
                  pure (Set.fromList (map PendingReviews.prAssignmentId stillFailing))

            postPendingReview p = do
              r <- tryCreateReview t
                     (PendingReviews.prAssignmentId p)
                     (PendingReviews.prWrongMeaning p)
                     (PendingReviews.prWrongReading p)
                     (PendingReviews.prCreatedAt p)
              pure (p, r)

        stillPendingIds <- retryPendingReviews

        let runBatch = do
              now2 <- getCurrentTime
              asAll <- Api.getAvailableAssignments t now2 n
              -- Assignments we already answered last run but couldn't submit
              -- (still stuck in the pending file after retryPendingReviews)
              -- stay "available" on WaniKani's side since they were never
              -- recorded -- exclude them so this session doesn't ask again
              -- for an answer it already has.
              let as = filter (\a -> not (Api.asId a `Set.member` stillPendingIds)) asAll
              if null as
                then putStrLn "No reviews available right now."
                else do
                  let subjectIds = map Api.asSubjectId as
                      subjToAsg  = M.fromList [ (Api.asSubjectId a, a) | a <- as ]
                  subjects <- applyStudyMaterials userSynonyms
                                <$> Api.getSubjectsByIds t subjectIds
                  allSubjMap <- fetchSubjectContext t userSynonyms subjects
                  let asgToInfo  = M.fromList
                        [ (Api.asId asg, (subj, asg))
                        | subj <- subjects
                        , Just asg <- [M.lookup (Api.subjId subj) subjToAsg]
                        ]
                  let audioPlayer = Config.cfgAudioPlayer cfg
                  let refreshSummary = do
                        now' <- getCurrentTime
                        summary' <- Api.getSummary t
                        pure (now', summary')
                  history <- History.loadHistory
                  let priorWrong = History.historyCounts history
                  putStrLn "[DEBUG] entering Tui.runStudyTui"
                  (wantsMore, sessionCounts) <-
                    Tui.runStudyTui
                      Tui.StudyConfig
                        { Tui.scRequeueAfter  = rqAfter
                        , Tui.scAudioPlayer   = audioPlayer
                        , Tui.scAudioAutoplay = audioAutoplay
                        , Tui.scPracticeOnly  = False
                        }
                      user summary now tz allSubjMap subjToAsg priorWrong subjects
                      refreshSummary (submitBatch asgToInfo) (submitSynonymWith t)
                  putStrLn ("[DEBUG] Tui.runStudyTui returned, wantsMore=" <> show wantsMore <> " sessionCounts=" <> show (fmap length sessionCounts))
                  now3 <- getCurrentTime
                  -- 'Nothing' means the batch was never submitted; don't
                  -- record its misses in the leech history.
                  case sessionCounts of
                    Nothing -> pure ()
                    Just cs -> History.saveHistory (History.mergeSession now3 cs history)
                  putStrLn "[DEBUG] History.saveHistory completed"
                  if wantsMore then runBatch else pure ()

            -- The actual submission work, isolated so 'submitBatch' can wrap
            -- it in its own 'try' -- everything here runs inside a forked
            -- thread with no other safety net, and 'PendingReviews.
            -- savePendingReviews' only catches 'IOException' specifically,
            -- so any other exception type (a bug, an unexpected library
            -- exception, anything not anticipated) would otherwise abort
            -- silently with no record of where it happened.
            submitBatchCore asgToInfo ts subs = do
              outcomes <- mapConcurrentlyN maxParallelSubmits (postReview ts) subs
              debugLog ("[DEBUG] mapConcurrentlyN returned " <> show (length outcomes) <> " outcomes")
              let details   = map (fmtSub asgToInfo) outcomes
                  succeeded = length [() | (_, Right _) <- outcomes]
                  failed    = length outcomes - succeeded
                  newlyFailed =
                    [ PendingReviews.PendingReview (Tui.subAssignmentId s) (Tui.subWrongMeaning s) (Tui.subWrongReading s) ts
                    | (s, Left _) <- outcomes
                    ]
              debugLog ("[DEBUG] succeeded=" <> show succeeded <> " failed=" <> show failed <> " newlyFailed=" <> show (length newlyFailed))
              -- Some of these "failures" may actually have been recorded by
              -- WaniKani already (the response was lost after a successful
              -- POST) -- don't queue those for a retry that can only fail.
              (genuinelyFailed, droppedN) <- dropAlreadyRecorded newlyFailed
              debugLog ("[DEBUG] after availability check: genuinelyFailed=" <> show (length genuinelyFailed) <> " droppedN=" <> show droppedN)
              let submitMsg = "Submitted " <> show succeeded
                           <> (if failed == 0
                                then ""
                                else " (" <> show failed <> " failed"
                                  <> (if droppedN == 0
                                        then ""
                                        else ", " <> show droppedN <> " already recorded on WaniKani")
                                  <> (if null genuinelyFailed
                                        then ""
                                        else ", saved for automatic retry next run")
                                  <> ")")
              if null genuinelyFailed
                then pure ()
                else do
                  path <- PendingReviews.pendingReviewsPath
                  debugLog ("[DEBUG] about to save " <> show (length genuinelyFailed) <> " pending reviews to " <> path)
                  existingPending <- PendingReviews.loadPendingReviews
                  debugLog ("[DEBUG] loaded " <> show (length existingPending) <> " existing pending reviews")
                  PendingReviews.savePendingReviews (PendingReviews.addPending existingPending genuinelyFailed)
                  debugLog "[DEBUG] savePendingReviews call completed"
              pure (details, submitMsg)

            submitBatch asgToInfo subs = do
              debugLog ("[DEBUG] submitBatch starting, " <> show (length subs) <> " submissions")
              ts <- getCurrentTime
              coreResult <- try (submitBatchCore asgToInfo ts subs)
                              :: IO (Either SomeException ([String], String))
              case coreResult of
                Left e -> do
                  debugLog ("[DEBUG] submitBatch CORE THREW: " <> shortErr e)
                  pure Tui.SubmitResult
                    { Tui.srMessage = "Submit failed unexpectedly: " <> shortErr e
                    , Tui.srHasMore = False
                    , Tui.srDetails = []
                    }
                Right (details, submitMsg) -> do
                  now2 <- getCurrentTime
                  -- The reviews above are already posted; don't let a
                  -- network blip on this trailing status check throw away
                  -- those per-item results (that used to surface as a bare
                  -- "submit failed" with no details at all).
                  statusResult <- try (do
                    summary2 <- Api.getSummary t
                    as2      <- Api.getAvailableAssignments t now2 n
                    pure (summary2, as2)) :: IO (Either SomeException (Api.Summary, [Api.Assignment]))
                  let (msg, hasMore) = case statusResult of
                        Right (summary2, as2) ->
                          ( submitMsg <> ". Reviews available now: "
                              <> show (Api.reviewsAvailableNow now2 summary2)
                          , not (null as2)
                          )
                        Left e ->
                          ( submitMsg <> ". (could not refresh review count: " <> shortErr e <> ")"
                          , False
                          )
                  pure Tui.SubmitResult
                    { Tui.srMessage = msg
                    , Tui.srHasMore = hasMore
                    , Tui.srDetails = details
                    }

            postReview ts s = do
              r <- tryCreateReview t
                     (Tui.subAssignmentId s)
                     (Tui.subWrongMeaning s)
                     (Tui.subWrongReading s)
                     ts
              case r of
                Left e -> debugLog ("[DEBUG] postReview FAILED for assignment " <> show (Tui.subAssignmentId s) <> ": " <> shortErr e)
                Right _ -> pure ()
              pure (s, r)

        runBatch

  case Cli.optCommand opts of
    Cli.Init -> Config.initConfig

    Cli.WhoAmI -> do
      t <- requireToken
      user <- Api.getUser t
      putStrLn ("Username: " <> T.unpack (Api.userUsername user))
      putStrLn ("Level:    " <> show (Api.userLevel user))
      putStrLn ("Profile:  " <> T.unpack (Api.userProfileUrl user))

    Cli.Reviews -> do
      t <- requireToken
      now <- getCurrentTime
      tz  <- getCurrentTimeZone
      summary <- Api.getSummary t

      putStrLn "Hour (local)           New  Open"
      putStrLn "---------------------------------"

      let rows = Api.reviewsPerHourNext24 now summary
          fmtHour utc =
            let lt = utcToLocalTime tz utc
            in formatTime defaultTimeLocale "%F %H:00" lt

      mapM_
        (\(hStart, newN, openN) ->
          putStrLn
            ( strPadRight 20 (fmtHour hStart) <> "  "
           <> strPadLeft 3 (show newN) <> "  "
           <> strPadLeft 4 (show openN)
            )
        )
        rows

    Cli.Leeches leechOpts -> do
      t <- requireToken
      history <- History.loadHistory
      if M.null (History.activeLeeches history)
        then putStrLn "No leeches tracked yet."
        else if Cli.leechesStudy leechOpts
          then runLeechStudy t history
          else runLeechList t history

    Cli.Study studyOpts -> runStudy studyOpts

-- | Attach the user's own WaniKani study-material data (meaning synonyms and
-- the record id) to each subject, so 'Tui.acceptedMeanings' treats the
-- synonyms as correct answers -- the user added them precisely so WaniKani
-- would accept them -- and 'Api.putMeaningSynonyms' can update the right
-- record when a new synonym is added.
applyStudyMaterials :: M.Map Api.SubjectId Api.StudyMaterial -> [Api.Subject] -> [Api.Subject]
applyStudyMaterials sms = map patch
  where
    patch s = case M.lookup (Api.subjId s) sms of
      Just sm -> s { Api.subjUserSynonyms    = Api.smMeaningSynonyms sm
                   , Api.subjStudyMaterialId = Just (Api.smId sm)
                   }
      Nothing -> s

-- | The synonym-submission callback threaded into 'Tui.runStudyTui': add a
-- meaning synonym to WaniKani for a subject (create or update as needed).
submitSynonymWith :: String -> Api.SubjectId -> Maybe Api.StudyMaterialId -> [T.Text] -> IO Api.StudyMaterial
submitSynonymWith t sid mSmId synonyms = Api.putMeaningSynonyms t mSmId sid synonyms

-- | Fetch the extra subjects referenced by a batch of subjects being
-- studied -- component kanji/radicals, vocabulary that uses a kanji, and
-- visually-similar kanji -- and build a lookup map covering both the batch
-- itself and everything referenced from it. Used to render the Ctrl-a
-- overlay and wrong-answer hints; shared by a normal study batch and a
-- leech practice round, which need identical reference data.
fetchSubjectContext :: String -> M.Map Api.SubjectId Api.StudyMaterial -> [Api.Subject] -> IO (M.Map Api.SubjectId Api.Subject)
fetchSubjectContext t sms subjects = do
  let compIds = nub [ cid | s <- subjects, cid <- Api.subjComponentIds s ]
  compSubjects <- applyStudyMaterials sms <$> Api.getSubjectsByIds t compIds
  let amalgIds = nub
        [ aid
        | s <- subjects
        , Api.subjType s == Api.Kanji
        , aid <- Api.subjAmalgamationIds s
        ]
  amalgSubjects <- applyStudyMaterials sms <$> Api.getSubjectsByIds t amalgIds
  let simIds = nub
        [ vid
        | s <- subjects
        , Api.subjType s == Api.Kanji
        , vid <- Api.subjVisuallySimilarIds s
        ]
  simSubjects <- applyStudyMaterials sms <$> Api.getSubjectsByIds t simIds
  pure $ M.fromList
    [ (Api.subjId s, s)
    | s <- subjects ++ compSubjects ++ amalgSubjects ++ simSubjects
    ]

-- | 'Api.createReview', caught: shared by a fresh batch submission
-- ('postReview') and a resubmit of a previously-failed one
-- ('postPendingReview'), which differ only in where the four arguments
-- come from ('Tui.Submission' vs. 'PendingReviews.PendingReview').
tryCreateReview :: String -> Api.AssignmentId -> Int -> Int -> UTCTime -> IO (Either SomeException Api.ReviewResult)
tryCreateReview t assignmentId wrongMeaning wrongReading createdAt =
  trySync (Api.createReview t assignmentId wrongMeaning wrongReading createdAt)

-- | "name  status" for a submission, e.g. "字 (word)  incorrect (m:1 r:0)".
-- Shared base for 'fmtSub' (appends the resulting WaniKani SRS stage) and
-- 'fmtLeechPractice' (a practice round has no WaniKani submission, so
-- nothing to append).
fmtSubBase :: M.Map Api.AssignmentId (Api.Subject, Api.Assignment) -> Tui.Submission -> String
fmtSubBase asgToInfo s =
  let wrongTotal = Tui.subWrongMeaning s + Tui.subWrongReading s
      name =
        case M.lookup (Tui.subAssignmentId s) asgToInfo of
          Just (subj, _) -> subjLabel subj
          Nothing        -> "assignment #" <> show (Tui.subAssignmentId s)
      status
        | wrongTotal == 0 = "correct"
        | otherwise       = "incorrect"
                         <> " (m:" <> show (Tui.subWrongMeaning s)
                         <> " r:" <> show (Tui.subWrongReading s) <> ")"
  in name <> "  " <> status

fmtSub
  :: M.Map Api.AssignmentId (Api.Subject, Api.Assignment)
  -> (Tui.Submission, Either SomeException Api.ReviewResult)
  -> String
fmtSub asgToInfo (s, eResult) =
  fmtSubBase asgToInfo s <> stageSuffix
  where
    stageSuffix = case eResult of
      Right rr -> " → " <> Api.srsStageLabel (Api.rrEndingSrsStage rr)
      Left  e  -> " (failed: " <> shortErr e <> ")"

-- | Like 'fmtSub', for a leech-only practice round: no WaniKani submission
-- happened, so there is no ending SRS stage to report.
fmtLeechPractice
  :: M.Map Api.AssignmentId (Api.Subject, Api.Assignment)
  -> Tui.Submission
  -> String
fmtLeechPractice = fmtSubBase

leechTotal :: History.LeechEntry -> Int
leechTotal = History.leechWeight

fmtLeechRow :: TimeZone -> M.Map Api.SubjectId Api.Subject -> History.LeechEntry -> String
fmtLeechRow tz subjMap e =
  let sid   = History.leSubjectId e
      label = case M.lookup sid subjMap of
                Just s  -> subjLabel s
                Nothing -> "subject #" <> show sid
      lvl   = case M.lookup sid subjMap of
                Just s  -> show (Api.subjLevel s)
                Nothing -> "?"
      lastSeen = formatTime defaultTimeLocale "%F %H:%M"
                   (utcToLocalTime tz (History.leLastSeen e))
  in strPadRight 26 label <> "  "
  <> strPadLeft 3 lvl <> "  "
  <> strPadLeft 7 (show (History.leWrongMeaning e)) <> "  "
  <> strPadLeft 7 (show (History.leWrongReading e)) <> "  "
  <> lastSeen

subjLabel :: Api.Subject -> String
subjLabel subj =
  let chars = case Api.subjChars subj of
                Just c | not (T.null (T.strip c)) -> T.unpack c
                _ -> ""
      meaning = case Api.subjMeanings subj of
                  (m:_) -> T.unpack m
                  []    -> "?"
  in if null chars then meaning else chars <> " (" <> meaning <> ")"

-- | Stay well under WaniKani's 60 req/min budget even on big batches.
maxParallelSubmits :: Int
maxParallelSubmits = 50

-- | Like 'mapConcurrently', but cap the number of in-flight actions at @n@.
mapConcurrentlyN :: Int -> (a -> IO b) -> [a] -> IO [b]
mapConcurrentlyN n f xs = do
  sem <- newQSem n
  mapConcurrently (\x -> bracket_ (waitQSem sem) (signalQSem sem) (f x)) xs
