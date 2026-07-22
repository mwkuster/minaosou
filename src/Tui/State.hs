{-# LANGUAGE OverloadedStrings #-}

module Tui.State
  ( -- Types
    Name(..)
  , Overlay(..)
  , Mode(..)
  , QKind(..)
  , Q(..)
  , Submission(..)
  , SubmitResult(..)
  , AppEvent(..)
  , Progress(..)
  , AppState(..)
  , StudyConfig(..)

    -- Session logic
  , currentQuestion
  , infoQuestion
  , submitAnswer
  , advanceCorrect
  , advanceOverride
  , requeueWrong
  , requeueOnly
  , requeueAfterK
  , mkQueueWidget
  , overlayViewport

    -- Progress / submissions
  , markOk
  , incWrong
  , mkSubmissions
  , sessionWrongCounts
  , recordableWrongCounts
  , initProgress

    -- Setup
  , mkQuestions
  , spaceOutSameSubject
  , acceptedReadings

    -- Answer checking / display
  , checkAnswer
  , acceptedMeanings
  , normMeaning
  , normReading
  , britishToAmerican
  , displayItem
  , displayCore
  , displayTag
  , kindLabel
  , displayInput
  , hasAudio
  , shouldAutoplay
  , markAutoplayed
  , missedBeforeLabel
  ) where

import qualified Api
import qualified Romaji

import Brick.BChan (BChan)
import qualified Brick.Widgets.List as L
import Control.Exception (SomeException)
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, isJust)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.LocalTime (TimeZone)
import qualified Data.Vector as Vec

--------------------------------------------------------------------------------
-- Public data
--------------------------------------------------------------------------------

data QKind = QMeaning | QReading
  deriving (Show, Eq, Ord)

data Q = Q
  { qSubject :: Api.Subject
  , qKind    :: QKind
  } deriving (Show, Eq)

data Submission = Submission
  { subAssignmentId :: Api.AssignmentId
  , subWrongMeaning :: Int
  , subWrongReading :: Int
  } deriving (Show, Eq)

data SubmitResult = SubmitResult
  { srMessage :: String
  , srHasMore :: Bool
  , srDetails :: [String]   -- per-submission lines for TUI display
  } deriving (Show)

-- | Session-wide settings for 'Tui.runStudyTui', grouped into one record so
-- a new setting doesn't grow the function's positional-argument list
-- further and similarly-typed flags (several 'Bool's) can't be swapped by
-- position at a call site.
data StudyConfig = StudyConfig
  { scRequeueAfter  :: Int
  , scAudioPlayer   :: Maybe String
  , scAudioAutoplay :: Bool
  , scPracticeOnly  :: Bool
  } deriving (Show, Eq)

data Progress = Progress
  { pMeaningOk     :: Bool
  , pReadingNeeded :: Bool
  , pReadingOk     :: Bool
  , pMeaningWrong  :: Int
  , pReadingWrong  :: Int
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- TUI state
--------------------------------------------------------------------------------

data Name = QueueList | InfoViewport | UserViewport | ReviewViewport | DoneViewport | MainViewport
  deriving (Ord, Eq, Show)

data Overlay = NoOverlay | AllInfo | UserInfo | ReviewSchedule
  deriving (Show, Eq)

data Mode
  = Normal
  | WrongAnswer Text [String]  -- user's input, accepted answers
  | Feedback Text
  | ConfirmSubmit
  | Submitting                 -- background submission in flight; input blocked
  | Finished
  deriving (Show, Eq)

-- | Custom Brick event injected from background threads.
data AppEvent = SubmitDone (Either SomeException SubmitResult)

data AppState = AppState
  { stQueue        :: [Q]
  , stQueueWidget  :: L.List Name Q
  , stInput        :: Text
  , stProgress     :: M.Map Api.SubjectId Progress
  , stSubjToAsg    :: M.Map Api.SubjectId Api.Assignment
  , stRequeueAfter :: Int
  , stCorrect      :: Int
  , stWrong        :: Int
  , stOverridden   :: Int
  , stMode         :: Mode
  , stBanner       :: Maybe Text
  , stError        :: Maybe Text                       -- transient error message (network etc.)
  , stHasMore      :: Bool
  , stWantsMore    :: Bool
  , stAudioPlayer   :: Maybe String                    -- command to play audio (e.g. "mpv --really-quiet")
  , stSubmitDetails :: [String]                        -- per-submission lines shown after submit
  , stOverlay       :: Overlay                         -- active info overlay
  , stAllSubjects   :: M.Map Api.SubjectId Api.Subject -- full subject map incl. components
  , stUser          :: Api.User
  , stSummary       :: Api.Summary
  , stNow           :: UTCTime
  , stTZ            :: TimeZone
  , stSubmitChan    :: BChan AppEvent                  -- background submission notifications
  , stLastCompleted :: Maybe Q                         -- last question to leave the queue head
  , stPriorWrong    :: M.Map Api.SubjectId (Int, Int)  -- cross-session wrong counts, read-only this session
  , stAudioAutoplay :: Bool                            -- auto-play reading audio on first appearance (config)
  , stAutoplayed    :: S.Set Api.SubjectId             -- subjects already auto-played this session
  , stPracticeOnly  :: Bool                            -- leech practice session: never submitted to WaniKani
  , stSubmitAttempted :: Bool
    -- ^ The user confirmed a submission and it ran to completion (whether
    -- or not every individual review reached WaniKani -- failures are
    -- persisted for retry). Gates 'recordableWrongCounts' so an abandoned
    -- session doesn't write leech counts for reviews nobody recorded.
  }

--------------------------------------------------------------------------------
-- Session logic
--------------------------------------------------------------------------------

currentQuestion :: AppState -> Maybe Q
currentQuestion st =
  case stQueue st of
    []    -> Nothing
    (q:_) -> Just q

-- | Question to display in the all-info overlay. In WrongAnswer mode the
-- user is still acting on the current item, so always show that. Otherwise,
-- if no input has been typed yet for the next question, prefer the last
-- completed question so the user can review what they just answered.
infoQuestion :: AppState -> Maybe Q
infoQuestion st =
  case stMode st of
    WrongAnswer _ _ -> currentQuestion st
    _ ->
      case stLastCompleted st of
        Just q | T.null (stInput st) -> Just q
        _                            -> currentQuestion st

submitAnswer :: Q -> Text -> AppState -> AppState
submitAnswer q answer st =
  let (ok, expected) = checkAnswer q answer
  in if ok
       then advanceCorrect q st
       else st
          { stInput = T.empty
          , stMode  = WrongAnswer answer expected
          }

advanceCorrect :: Q -> AppState -> AppState
advanceCorrect q st =
  let prog'  = markOk (qSubject q) (qKind q) (stProgress st)
      queue' = drop 1 (stQueue st)
  in st
     { stQueue         = queue'
     , stQueueWidget   = mkQueueWidget queue'
     , stProgress      = prog'
     , stCorrect       = stCorrect st + 1
     , stInput         = T.empty
     , stMode          = if null queue' then Finished else Feedback "✓"
     , stLastCompleted = Just q
     }

advanceOverride :: Q -> AppState -> AppState
advanceOverride q st =
  let prog'  = markOk (qSubject q) (qKind q) (stProgress st)
      queue' = drop 1 (stQueue st)
  in st
     { stQueue         = queue'
     , stQueueWidget   = mkQueueWidget queue'
     , stProgress      = prog'
     , stCorrect       = stCorrect st + 1
     , stOverridden    = stOverridden st + 1
     , stInput         = T.empty
     , stMode          = if null queue' then Finished else Feedback "override"
     , stLastCompleted = Just q
     }

requeueWrong :: Q -> AppState -> AppState
requeueWrong q st =
  let prog'  = incWrong (qSubject q) (qKind q) (stProgress st)
      queue' = requeueAfterK (stRequeueAfter st) q (drop 1 (stQueue st))
  in st
     { stQueue         = queue'
     , stQueueWidget   = mkQueueWidget queue'
     , stProgress      = prog'
     , stWrong         = stWrong st + 1
     , stInput         = T.empty
     , stMode          = Feedback "requeued"
     , stLastCompleted = Just q
     }

-- | Requeue without recording a wrong answer (no penalty to wrong counts).
requeueOnly :: Q -> AppState -> AppState
requeueOnly q st =
  let queue' = requeueAfterK (stRequeueAfter st) q (drop 1 (stQueue st))
  in st
     { stQueue         = queue'
     , stQueueWidget   = mkQueueWidget queue'
     , stInput         = T.empty
     , stMode          = Feedback "requeued"
     , stLastCompleted = Just q
     }

requeueAfterK :: Int -> Q -> [Q] -> [Q]
requeueAfterK k q qs =
  let k' = max 0 k
      (front, back) = splitAt k' qs
  in front ++ [q] ++ back

mkQueueWidget :: [Q] -> L.List Name Q
mkQueueWidget qs =
  L.list QueueList (Vec.fromList qs) 1

-- | The scrollable viewport belonging to an overlay, if it has one.
-- 'NoOverlay' has none -- expressed as 'Nothing' rather than as an
-- unreachable 'error', so a future caller that forgets the guard scrolls
-- nothing instead of taking down the whole TUI.
overlayViewport :: Overlay -> Maybe Name
overlayViewport o =
  case o of
    AllInfo        -> Just InfoViewport
    UserInfo       -> Just UserViewport
    ReviewSchedule -> Just ReviewViewport
    NoOverlay      -> Nothing

--------------------------------------------------------------------------------
-- Progress / submissions
--------------------------------------------------------------------------------

markOk :: Api.Subject -> QKind -> M.Map Api.SubjectId Progress -> M.Map Api.SubjectId Progress
markOk subj kind mp =
  let sid = Api.subjId subj
  in M.adjust upd sid mp
  where
    upd p =
      case kind of
        QMeaning -> p { pMeaningOk = True }
        QReading -> p { pReadingOk = True }

incWrong :: Api.Subject -> QKind -> M.Map Api.SubjectId Progress -> M.Map Api.SubjectId Progress
incWrong subj kind mp =
  let sid = Api.subjId subj
  in M.adjust upd sid mp
  where
    upd p =
      case kind of
        QMeaning -> p { pMeaningWrong = pMeaningWrong p + 1 }
        QReading -> p { pReadingWrong = pReadingWrong p + 1 }

mkSubmissions :: AppState -> [Submission]
mkSubmissions st =
  [ Submission
      { subAssignmentId = asgId
      , subWrongMeaning = pMeaningWrong p
      , subWrongReading = pReadingWrong p
      }
  | (sid, p) <- M.toList (stProgress st)
  , Just asg <- [M.lookup sid (stSubjToAsg st)]
  , let asgId = Api.asId asg
  ]

-- | This session's wrong-answer counts, one entry per subject that was
-- missed at least once (clean subjects are omitted).
sessionWrongCounts :: AppState -> [(Api.SubjectId, Int, Int)]
sessionWrongCounts st =
  [ (sid, pMeaningWrong p, pReadingWrong p)
  | (sid, p) <- M.toList (stProgress st)
  , pMeaningWrong p > 0 || pReadingWrong p > 0
  ]

-- | The wrong-answer counts that may be written to the cross-session leech
-- history: this session's, but only if the user actually went through with
-- submitting the batch.
--
-- Quitting mid-session leaves the reviews unrecorded on WaniKani, so the
-- very same items come back in the next run. Persisting their misses anyway
-- meant the same mistake was counted once per abandoned attempt, inflating
-- a subject's leech weight without the user ever having missed it twice.
recordableWrongCounts :: AppState -> [(Api.SubjectId, Int, Int)]
recordableWrongCounts st
  | stSubmitAttempted st = sessionWrongCounts st
  | otherwise            = []

--------------------------------------------------------------------------------
-- Setup helpers
--------------------------------------------------------------------------------

mkQuestions :: Api.Subject -> [Q]
mkQuestions s =
  let rs = acceptedReadings s
  in Q s QMeaning
     : [ Q s QReading
       | Api.subjType s /= Api.Radical
       , not (null rs)
       ]

-- | Rearrange so meaning/reading questions for the same subject never land
-- adjacent to each other (a flat shuffle can place them back-to-back by
-- chance, which defeats the within-session spacing effect).
spaceOutSameSubject :: [Q] -> [Q]
spaceOutSameSubject = go
  where
    go (x : y : rest)
      | sameSubject x y =
          case break (not . sameSubject x) rest of
            (same, z : zs) -> x : z : go (y : same ++ zs)
            (_,    [])     -> x : y : rest
      | otherwise = x : go (y : rest)
    go xs = xs

    sameSubject a b = Api.subjId (qSubject a) == Api.subjId (qSubject b)

initProgress :: Api.Subject -> Progress
initProgress s =
  let needsReading =
        Api.subjType s /= Api.Radical
        && not (null (acceptedReadings s))
  in Progress False needsReading False 0 0

acceptedReadings :: Api.Subject -> [Text]
acceptedReadings s =
  filter (not . T.null . T.strip) (Api.subjReadings s)

--------------------------------------------------------------------------------
-- Answer checking / display
--------------------------------------------------------------------------------

-- | Every answer that counts as a correct meaning, matching what WaniKani
-- itself accepts in a review: its primary meanings, its whitelisted
-- auxiliary meanings, and the user's own synonyms -- minus anything
-- WaniKani explicitly blacklists.
--
-- Checking against 'Api.subjMeanings' alone (as this used to) makes kroki
-- stricter than WaniKani: roughly a third of subjects carry a whitelisted
-- alternative, so a legitimate answer was marked wrong, requeued, recorded
-- as a leech, /and/ submitted back to WaniKani as incorrect -- actively
-- lowering the SRS stage for an answer WaniKani would have accepted.
acceptedMeanings :: Api.Subject -> [Text]
acceptedMeanings subj =
  [ m
  | m <- Api.subjMeanings subj
      ++ Api.subjAuxWhitelist subj
      ++ Api.subjUserSynonyms subj
  , normMeaning m `notElem` blacklisted
  ]
  where blacklisted = map normMeaning (Api.subjAuxBlacklist subj)

checkAnswer :: Q -> Text -> (Bool, [String])
checkAnswer (Q subj kind) ans =
  case kind of
    QMeaning ->
      let acceptedNorm = map normMeaning (acceptedMeanings subj)
      in ( normMeaning ans `elem` acceptedNorm
         -- Show only the canonical meanings: the alternatives are accepted
         -- silently, but WaniKani teaches these.
         , map T.unpack (Api.subjMeanings subj)
         )
    QReading ->
      let rs = acceptedReadings subj
          acceptedNorm = map normReading rs
      in ( normReading ans `elem` acceptedNorm
         , map T.unpack rs
         )

displayItem :: Api.Subject -> String
displayItem s = displayCore s <> displayTag s

-- | Just the character(s) (or meaning fallback), with no type tag.
displayCore :: Api.Subject -> String
displayCore s =
  case Api.subjChars s of
    Just c | let cs = T.strip c, not (T.null cs) -> T.unpack cs
    _ ->
      let m = case Api.subjMeanings s of
                (x:_) -> T.unpack x
                []    -> "?"
      in m <> " (#" <> show (Api.subjId s) <> ")"

-- | The trailing " (Kanji)" / " (Radical)" / " (Vocab)" annotation.
displayTag :: Api.Subject -> String
displayTag s =
  case Api.subjType s of
    Api.Kanji          -> " (Kanji)"
    Api.Radical        -> " (Radical)"
    Api.Vocabulary     -> " (Vocab)"
    Api.KanaVocabulary -> " (Vocab)"

kindLabel :: QKind -> String
kindLabel QMeaning = "meaning"
kindLabel QReading = "reading"

displayInput :: QKind -> Text -> Text
displayInput QReading t = Romaji.romajiToHiraganaLive t
displayInput QMeaning t = t

hasAudio :: Q -> AppState -> Bool
hasAudio q st =
  not (null (Api.subjAudioUrls (qSubject q))) && isJust (stAudioPlayer st)

-- | Whether a question's audio should be auto-played on first appearance:
-- reading questions on vocab, with audio configured, not already played
-- this session, and the feature enabled.
shouldAutoplay :: AppState -> Q -> Bool
shouldAutoplay st q =
  stAudioAutoplay st
  && qKind q == QReading
  && Api.subjType (qSubject q) `elem` [Api.Vocabulary, Api.KanaVocabulary]
  && hasAudio q st
  && not (S.member (Api.subjId (qSubject q)) (stAutoplayed st))

markAutoplayed :: Q -> AppState -> AppState
markAutoplayed q st =
  st { stAutoplayed = S.insert (Api.subjId (qSubject q)) (stAutoplayed st) }

-- | "Missed before" label for the all-info overlay, from cross-session
-- wrong counts. Nothing when the subject has never been missed.
missedBeforeLabel :: (Int, Int) -> Maybe String
missedBeforeLabel (mw, rw)
  | mw <= 0 && rw <= 0 = Nothing
  | otherwise = Just $ intercalate ", " $ catMaybes
      [ if mw > 0 then Just ("meaning ×" <> show mw) else Nothing
      , if rw > 0 then Just ("reading ×" <> show rw) else Nothing
      ]

normMeaning :: Text -> Text
normMeaning = collapseSpaces . britishToAmerican . T.toCaseFold . T.strip

-- | Convert British English spellings to American English, word by word.
-- Applied after case-folding so all lookups are lowercase.
britishToAmerican :: Text -> Text
britishToAmerican = T.unwords . map convertWord . T.words
  where
    convertWord w = M.findWithDefault (applySuffixRules w) w wordTable

    -- Word-pair table for cases that don't follow simple suffix rules.
    -- All keys must be lowercase (applied after toCaseFold).
    wordTable :: M.Map Text Text
    wordTable = M.fromList $
         re "centre"   "center"
      ++ re "theatre"  "theater"
      ++ re "fibre"    "fiber"
      ++ re "litre"    "liter"
      ++ re "metre"    "meter"
      ++ re "spectre"  "specter"
      ++ re "sabre"    "saber"
      ++ re "calibre"  "caliber"
      ++ re "lustre"   "luster"
      ++ re "sombre"   "somber"
      ++ [ ("defence",  "defense"),  ("defences",  "defenses")
         , ("offence",  "offense"),  ("offences",  "offenses")
         , ("pretence", "pretense"), ("pretences", "pretenses")
         , ("licence",  "license"),  ("licences",  "licenses")
         , ("practise", "practice")
         ]
      where
        re b a = [(b, a), (b <> "s", a <> "s")]

    applySuffixRules w
      | "iour"    `T.isSuffixOf` w                  = T.dropEnd 4 w <> "ior"
      | "ourable" `T.isSuffixOf` w                  = T.dropEnd 7 w <> "orable"
      | "ourably" `T.isSuffixOf` w                  = T.dropEnd 7 w <> "orably"
      | "ourite"  `T.isSuffixOf` w                  = T.dropEnd 6 w <> "orite"
      | "ourhood" `T.isSuffixOf` w                  = T.dropEnd 7 w <> "orhood"
      | "our"     `T.isSuffixOf` w
      , w `notElem` ourBlacklist                     = T.dropEnd 3 w <> "or"
      | "yse"     `T.isSuffixOf` w                  = T.dropEnd 3 w <> "yze"
      | "isation" `T.isSuffixOf` w                  = T.dropEnd 7 w <> "ization"
      | "ise"     `T.isSuffixOf` w
      , w `notElem` iseBlacklist                     = T.dropEnd 3 w <> "ize"
      | "ogue"    `T.isSuffixOf` w
      , w `notElem` ogueBlacklist                    = T.dropEnd 4 w <> "og"
      | otherwise                                    = w

    ourBlacklist =
      [ "four", "pour", "hour", "your", "sour", "dour", "tour", "flour"
      , "amour", "contour", "detour", "velour", "troubadour", "paramour" ]

    iseBlacklist =
      [ "rise", "wise", "guise", "surprise", "revise", "advise", "devise"
      , "enterprise", "exercise", "franchise", "improvise", "promise"
      , "supervise", "advertise", "comprise", "disguise", "arise"
      , "otherwise", "likewise", "clockwise", "lengthwise"
      , "prise", "demise", "surmise", "premise", "treatise"
      , "precise", "concise"
      , "noise", "poise", "turquoise", "tortoise", "porpoise" ]

    ogueBlacklist =
      [ "rogue", "vogue", "pirogue", "brogue" ]

normReading :: Text -> Text
normReading t =
  let t' = T.strip t
  in if T.all (\c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '\'') t'
       then Romaji.romajiToHiragana (T.toCaseFold t')
       else T.toCaseFold t'

collapseSpaces :: Text -> Text
collapseSpaces =
  T.unwords . filter (not . T.null) . T.words
