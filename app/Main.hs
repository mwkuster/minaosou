module Main (main) where

import qualified Cli
import qualified Api
import qualified Config
import qualified History
import qualified PendingReviews
import qualified Srs
import qualified Tui
import Util (groupDigits, median, shortErr, strPadLeft, strPadRight, trySync)

import Control.Applicative ((<|>))
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (SomeException, bracket_, try)
import System.Environment (lookupEnv)
import System.Exit (die)

import Data.Time (addUTCTime, diffUTCTime, getCurrentTime, getCurrentTimeZone, utcToLocalTime, TimeZone, UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.List (nub, partition, sort, sortOn)
import Data.Maybe (fromMaybe, isJust)
import Numeric (showFFloat)
import System.IO (hFlush, hPutStr, hPutStrLn, stderr)
import Data.Map.Strict qualified as M
import qualified Data.Set as Set
import qualified Data.Text as T

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
          (die "Missing API token. Provide --token, set WANIKANI_API_TOKEN, or put token=... into ~/.config/minaosou/config")
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
                  -- The availability check is itself a network call, and the
                  -- usual reason a batch failed is that the network is down --
                  -- in which case this call fails too. It must never abort the
                  -- caller, which is about to persist these failures for
                  -- retry: losing an answered review is far worse than a
                  -- redundant retry next run. So on any error, prune nothing
                  -- and treat every failure as genuine.
                  result <- trySync (Api.getStillAvailableAssignmentIds t now'
                              (map PendingReviews.prAssignmentId failed))
                  case result of
                    Left _              -> pure (failed, 0)
                    Right stillAvailable ->
                      let (genuine, alreadyRecorded) =
                            partition
                              (\p -> PendingReviews.prAssignmentId p `Set.member` stillAvailable)
                              failed
                      in pure (genuine, length alreadyRecorded)

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
                  now3 <- getCurrentTime
                  -- 'Nothing' means the batch was never submitted; don't
                  -- record its misses in the leech history.
                  case sessionCounts of
                    Nothing -> pure ()
                    Just cs -> History.saveHistory (History.mergeSession now3 cs history)
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
              let details   = map (fmtSub asgToInfo) outcomes
                  succeeded = length [() | (_, Right _) <- outcomes]
                  failed    = length outcomes - succeeded
                  newlyFailed =
                    [ PendingReviews.PendingReview (Tui.subAssignmentId s) (Tui.subWrongMeaning s) (Tui.subWrongReading s) ts
                    | (s, Left _) <- outcomes
                    ]
              -- Some of these "failures" may actually have been recorded by
              -- WaniKani already (the response was lost after a successful
              -- POST) -- don't queue those for a retry that can only fail.
              (genuinelyFailed, droppedN) <- dropAlreadyRecorded newlyFailed
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
                  existingPending <- PendingReviews.loadPendingReviews
                  PendingReviews.savePendingReviews (PendingReviews.addPending existingPending genuinelyFailed)
              pure (details, submitMsg)

            submitBatch asgToInfo subs = do
              ts <- getCurrentTime
              coreResult <- try (submitBatchCore asgToInfo ts subs)
                              :: IO (Either SomeException ([String], String))
              case coreResult of
                Left e ->
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

    Cli.Forecast forecastOpts -> do
      t <- requireToken
      runForecast t forecastOpts

    Cli.Study studyOpts -> runStudy studyOpts

--------------------------------------------------------------------------------
-- forecast
--------------------------------------------------------------------------------

-- | Project the daily review load a lesson pace settles at, from what the
-- account has actually cost so far.
--
-- The load is a consequence of two things the user controls: how many items
-- they start per day, and how often they answer wrong -- a miss is not just
-- a repeat, it drops the item down the ladder, so its remaining intervals
-- are paid over again. Both directions are reported, since "how many
-- lessons can I afford" and "how accurate do I need to be" are one question
-- asked from either end.
runForecast :: String -> Cli.ForecastOpts -> IO ()
runForecast t opts = do
  mSys <- Api.getSrsSystem t
  sys  <- maybe (die "WaniKani reported no spaced repetition system, so there is nothing to project against.") pure mSys

  stats <- Api.getReviewStatistics t (fetchProgress "answer record")
  hPutStrLn stderr ""
  prog  <- Api.getProgress t (fetchProgress "item progress")
  hPutStrLn stderr ""
  now <- getCurrentTime

  let burned  = Srs.burnedCohort prog stats
      studied = Srs.cohortBy (isJust . Api.pgStartedAt) prog stats
      lo      = Api.srsStartingStage sys
      hi      = Api.srsBurningStage sys - 1
      passing = Api.srsPassingStage sys
      passLbl = Api.srsStageNumLabel passing

  -- Two ways to pin the failure rate, in order of preference. What the
  -- burned items really cost is the stronger evidence -- it is the outcome
  -- itself rather than a proxy for it -- but nothing has burned on a young
  -- account, and then the answer counts are all there is.
  let fitted   = Srs.cohortMeanReviews burned >>= Srs.fitUniformFailure sys
      bracketed = fmap (\(a, b) -> (a + b) / 2) (Srs.itemFailureBracket studied)

  case fitted <|> bracketed of
    Nothing -> putStrLn "Nothing to measure yet -- do some reviews and try again."
    Just rate -> do
      let model = Srs.uniformModel sys rate
          pj    = Srs.project sys model

          windowDays = max 1 (Cli.forecastDays opts)
          cutoff     = addUTCTime (negate (fromIntegral windowDays * 86400)) now
          startedIn  = length [ () | p <- prog, Just s <- [Api.pgStartedAt p], s >= cutoff ]
          pace       = fromIntegral startedIn / fromIntegral windowDays :: Double
          lessons    = case Cli.forecastLessons opts of
                         Just n            -> fromIntegral (max 1 n)
                         Nothing | pace > 0 -> pace
                         Nothing            -> 10
          paceNote = case Cli.forecastLessons opts of
            Just _             -> ""
            Nothing | pace > 0 -> " (your own pace over the last " <> show windowDays <> " days)"
            Nothing            -> " (assumed -- you started no lessons in the last "
                                    <> show windowDays <> " days)"

          circulating = length [ () | p <- prog, inRange (Api.pgSrsStage p) ]
          belowPass   = length [ () | p <- prog, inRange (Api.pgSrsStage p)
                                    , Api.pgSrsStage p < passing ]
          inRange st  = st >= lo && st <= hi

          burnDays = median (sort
            [ realToFrac (diffUTCTime b st) / 86400
            | p <- prog, Just b <- [Api.pgBurnedAt p], Just st <- [Api.pgStartedAt p] ])

      putStrLn $
        "Measured from your own record: " <> groupDigits (Srs.cohItems studied)
        <> " items studied, " <> groupDigits (Srs.cohItems burned) <> " of them burned."
      putStrLn ""

      putStrLn "Answer accuracy"
      putStrLn $ accuracyRow "  everything you have studied" studied
      putStrLn $ accuracyRow "  just the items that burned " burned
      putStrLn $
        "\n  Burned items were learned at earlier levels, so they are the easier\n\
        \  half of your history. Where the two rows differ, the projection below\n\
        \  is the optimistic one."

      putStrLn ""
      putStrLn "What an item has cost you, lesson to burned"
      case Srs.cohortMeanReviews burned of
        Nothing -> putStrLn "  nothing has burned yet -- the figures below come from your answer counts instead"
        Just mean -> do
          putStrLn $ "  reviews per item          " <> strPadLeft 7 (fmt1 mean)
            <> maybe "" (\m -> "   (median " <> fmt0 m <> "; the gap is the leech tail)")
                     (Srs.cohortMedianReviews burned)
          case burnDays of
            Nothing -> pure ()
            Just d  -> putStrLn $ "  days per item             " <> strPadLeft 7 (fmt0 d) <> "   (median)"
      putStrLn $ "  miss rate per review      " <> strPadLeft 7 (pct rate)
        <> maybe ""
             (\(a, b) -> "   (answer counts bracket it at " <> pct a <> "-" <> pct b <> ")")
             (Srs.itemFailureBracket burned)

      putStrLn ""
      putStrLn $ "At " <> fmtPace lessons <> " lessons/day" <> paceNote <> ", that settles at"
      putStrLn ""
      putStrLn $ "  reviews per day           " <> strPadLeft 7 (fmt0 (lessons * Srs.pjReviewsPerItem pj))
      putStrLn $ "  days from lesson to burn  " <> strPadLeft 7 (fmt0 (Srs.pjDaysToBurn pj))
      putStrLn $ "  items in circulation      " <> strPadLeft 7 (fmt0 (lessons * Srs.pjDaysToBurn pj))
        <> "   (" <> fmt0 (lessons * apprenticeDays sys pj) <> " of them below " <> passLbl <> ")"
      putStrLn ""
      putStrLn $
        "  Your account holds " <> groupDigits circulating <> " items right now, "
        <> groupDigits belowPass <> " below " <> passLbl <> ", so at this\n  pace the load is still "
        <> (if fromIntegral circulating > lessons * Srs.pjDaysToBurn pj then "drifting down" else "climbing")
        <> " towards the figures above."

      putStrLn ""
      putStrLn "If your miss rate moved, at the same lesson pace"
      putStrLn ""
      putStrLn "  miss rate   reviews/day   lesson to burn"
      mapM_ (putStrLn . fmtScenario sys model lessons) [0.5, 0.75, 1.0, 1.25, 1.5]

      case Cli.forecastReviewsPerDay opts of
        Nothing -> pure ()
        Just budget -> do
          putStrLn ""
          putStrLn ("To hold " <> show budget <> " reviews/day instead, either")
          putStrLn ""
          putStrLn $
            "  · " <> fmt1 (fromIntegral budget / Srs.pjReviewsPerItem pj)
            <> " lessons/day at today's miss rate, or"
          putStrLn $
            "  · " <> case Srs.failureScaleFor sys model (fromIntegral budget / lessons) of
              Just k ->
                let scaled = Srs.scaleFailure k model
                in "a miss rate of " <> pct (Srs.overallFailure (Srs.project sys scaled) scaled)
                   <> " (from " <> pct rate <> "), keeping " <> fmtPace lessons <> " lessons/day"
              Nothing ->
                "no miss rate reaches it at " <> fmtPace lessons
                <> " lessons/day -- even a flawless run costs "
                <> fmt0 (lessons * fromIntegral (hi - lo + 1)) <> " reviews/day"
  where
    fetchProgress what n = do
      hPutStr stderr ("\rReading your " <> what <> "… " <> groupDigits n <> " records   ")
      hFlush stderr

-- | Expected days an item spends below the passing stage -- the Apprentice
-- churn. Those stages come back within hours, so they dominate what a day
-- actually feels like, far beyond their share of the items.
apprenticeDays :: Api.SrsSystem -> Srs.Projection -> Double
apprenticeDays sys pj =
  sum [ d | (s, d) <- Srs.pjDays pj, s < Api.srsPassingStage sys ]

accuracyRow :: String -> Srs.Cohort -> String
accuracyRow label c =
  label <> "  " <> strPadLeft 9 (groupDigits (Srs.cohReviews c)) <> " reviews"
  <> "   meaning " <> strPadLeft 6 (share (Srs.cohMeaningWrong c) (Srs.cohReviews c))
  <> "   reading " <> strPadLeft 6 (share (Srs.cohReadingWrong c) (Srs.cohReadingReviews c))
  <> " missed"
  where
    share _ 0 = "--"
    share x n = pct (fromIntegral x / fromIntegral n)

-- | One row of the sensitivity table: the fitted miss rate multiplied by a
-- factor, so the user can see what a given change of accuracy is worth
-- before deciding whether it is worth chasing.
fmtScenario :: Api.SrsSystem -> (Int -> Srs.StageModel) -> Double -> Double -> String
fmtScenario sys model lessons k =
  "  " <> strPadLeft 9 (pct (Srs.overallFailure pj scaled))
  <> strPadLeft 14 (fmt0 (lessons * Srs.pjReviewsPerItem pj))
  <> strPadLeft 17 (fmt0 (Srs.pjDaysToBurn pj) <> " days")
  <> (if k == 1.0 then "   <- now" else "")
  where
    scaled = Srs.scaleFailure k model
    pj     = Srs.project sys scaled

pct :: Double -> String
pct x = showFFloat (Just 1) (100 * x) "" <> "%"

fmt0 :: Double -> String
fmt0 x = groupDigits (round x)

fmt1 :: Double -> String
fmt1 x = showFFloat (Just 1) x ""

-- | A lesson pace: measured, so usually fractional, but "10 lessons/day"
-- reads better than "10.0" when the user asked for exactly that.
fmtPace :: Double -> String
fmtPace x
  | x == fromIntegral (round x :: Int) = show (round x :: Int)
  | otherwise                          = fmt1 x

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
-- studied -- component kanji/radicals, the radicals of those component
-- kanji, vocabulary that uses a kanji, and visually-similar kanji -- and
-- build a lookup map covering both the batch
-- itself and everything referenced from it. Used to render the Ctrl-a
-- overlay and wrong-answer hints; shared by a normal study batch and a
-- leech practice round, which need identical reference data.
fetchSubjectContext :: String -> M.Map Api.SubjectId Api.StudyMaterial -> [Api.Subject] -> IO (M.Map Api.SubjectId Api.Subject)
fetchSubjectContext t sms subjects = do
  let compIds = nub [ cid | s <- subjects, cid <- Api.subjComponentIds s ]
  compSubjects <- applyStudyMaterials sms <$> Api.getSubjectsByIds t compIds
  -- One level deeper: the radicals each component kanji is itself built
  -- from, so the Ctrl-a overlay can show a vocabulary's component kanji
  -- together with their radical composition. Ids already covered by the
  -- batch or the fetch above are dropped, so this is a small extra request
  -- (radicals repeat heavily across kanji).
  let known = Set.fromList (map Api.subjId (subjects ++ compSubjects))
      subCompIds = nub
        [ rid
        | c <- compSubjects
        , Api.subjType c == Api.Kanji
        , rid <- Api.subjComponentIds c
        , not (rid `Set.member` known)
        ]
  subCompSubjects <- applyStudyMaterials sms <$> Api.getSubjectsByIds t subCompIds
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
    | s <- subjects ++ compSubjects ++ subCompSubjects ++ amalgSubjects ++ simSubjects
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
