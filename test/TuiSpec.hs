{-# LANGUAGE OverloadedStrings #-}

module TuiSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Brick.Widgets.List as L
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)
import Data.Time.LocalTime (utc)

import qualified Api
import qualified Tui

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

sid :: Int -> Api.SubjectId
sid = Api.SubjectId

aid :: Int -> Api.AssignmentId
aid = Api.AssignmentId

-- Minimal kanji subject for testing
kanjiSubj :: Api.Subject
kanjiSubj = Api.Subject
  { Api.subjId              = sid 1
  , Api.subjType            = Api.Kanji
  , Api.subjLevel           = 1
  , Api.subjChars           = Just "日"
  , Api.subjMeanings        = ["Sun", "Day"]
  , Api.subjReadings        = ["にち", "じつ"]
  , Api.subjAudioUrls       = []
  , Api.subjMeaningMnemonic = Nothing
  , Api.subjReadingMnemonic = Nothing
  , Api.subjComponentIds    = []
  , Api.subjAmalgamationIds = []
  , Api.subjVisuallySimilarIds = []
  , Api.subjAuxWhitelist = []
  , Api.subjAuxBlacklist = []
  , Api.subjUserSynonyms = []
  , Api.subjStudyMaterialId = Nothing
  }

-- Radical (no reading question)
radicalSubj :: Api.Subject
radicalSubj = Api.Subject
  { Api.subjId              = sid 2
  , Api.subjType            = Api.Radical
  , Api.subjLevel           = 1
  , Api.subjChars           = Just "一"
  , Api.subjMeanings        = ["One"]
  , Api.subjReadings        = []
  , Api.subjAudioUrls       = []
  , Api.subjMeaningMnemonic = Nothing
  , Api.subjReadingMnemonic = Nothing
  , Api.subjComponentIds    = []
  , Api.subjAmalgamationIds = []
  , Api.subjVisuallySimilarIds = []
  , Api.subjAuxWhitelist = []
  , Api.subjAuxBlacklist = []
  , Api.subjUserSynonyms = []
  , Api.subjStudyMaterialId = Nothing
  }

-- Vocab subject
vocabSubj :: Api.Subject
vocabSubj = Api.Subject
  { Api.subjId              = sid 3
  , Api.subjType            = Api.Vocabulary
  , Api.subjLevel           = 3
  , Api.subjChars           = Just "学校"
  , Api.subjMeanings        = ["School"]
  , Api.subjReadings        = ["がっこう"]
  , Api.subjAudioUrls       = []
  , Api.subjMeaningMnemonic = Nothing
  , Api.subjReadingMnemonic = Nothing
  , Api.subjComponentIds    = []
  , Api.subjAmalgamationIds = []
  , Api.subjVisuallySimilarIds = []
  , Api.subjAuxWhitelist = []
  , Api.subjAuxBlacklist = []
  , Api.subjUserSynonyms = []
  , Api.subjStudyMaterialId = Nothing
  }

mkQ :: Api.Subject -> Tui.QKind -> Tui.Q
mkQ s k = Tui.Q { Tui.qSubject = s, Tui.qKind = k }

-- Bare AppState with only progress/subjToAsg populated (for mkSubmissions)
stateWith :: M.Map Api.SubjectId Tui.Progress -> M.Map Api.SubjectId Api.Assignment -> Tui.AppState
stateWith prog subjToAsg = Tui.AppState
  { Tui.stQueue        = []
  , Tui.stQueueWidget  = L.list Tui.QueueList (Vec.fromList []) 1
  , Tui.stInput        = ""
  , Tui.stProgress     = prog
  , Tui.stSubjToAsg    = subjToAsg
  , Tui.stRequeueAfter = 7
  , Tui.stCorrect      = 0
  , Tui.stWrong        = 0
  , Tui.stOverridden   = 0
  , Tui.stMode         = Tui.Normal
  , Tui.stBanner       = Nothing
  , Tui.stError        = Nothing
  , Tui.stNotice       = Nothing
  , Tui.stHasMore      = False
  , Tui.stWantsMore    = False
  , Tui.stAudioPlayer   = Nothing
  , Tui.stSubmitDetails = []
  , Tui.stOverlay       = Tui.NoOverlay
  , Tui.stAllSubjects   = M.empty
  , Tui.stUser          = Api.User { Api.userUsername = "test", Api.userLevel = 1, Api.userProfileUrl = "" }
  , Tui.stSummary       = Api.Summary { Api.summaryReviews = [] }
  , Tui.stNow           = UTCTime (fromGregorian 2024 1 1) (secondsToDiffTime 0)
  , Tui.stTZ            = utc
  , Tui.stSubmitChan    = error "stSubmitChan: not used in pure tests"
  , Tui.stLastCompleted = Nothing
  , Tui.stPriorWrong    = M.empty
  , Tui.stAudioAutoplay = False
  , Tui.stAutoplayed    = S.empty
  , Tui.stPracticeOnly  = False
  , Tui.stSubmitAttempted = True
  }

--------------------------------------------------------------------------------
-- Spec
--------------------------------------------------------------------------------

spec :: Spec
spec = do

  describe "normMeaning" $ do
    it "lowercases input"         $ Tui.normMeaning "Sun"        `shouldBe` "sun"
    it "trims whitespace"         $ Tui.normMeaning "  sun  "    `shouldBe` "sun"
    it "collapses inner spaces"   $ Tui.normMeaning "to  go"     `shouldBe` "to go"
    it "case-folds unicode"       $ Tui.normMeaning "GROß"       `shouldBe` "gross"
    it "empty string stays empty" $ Tui.normMeaning ""           `shouldBe` ""
    describe "british spellings" $ do
      it "colour → color"           $ Tui.normMeaning "colour"       `shouldBe` "color"
      it "honour → honor"           $ Tui.normMeaning "honour"       `shouldBe` "honor"
      it "honourable → honorable"   $ Tui.normMeaning "honourable"   `shouldBe` "honorable"
      it "behaviour → behavior"     $ Tui.normMeaning "behaviour"    `shouldBe` "behavior"
      it "centre → center"          $ Tui.normMeaning "centre"       `shouldBe` "center"
      it "theatre → theater"        $ Tui.normMeaning "theatre"      `shouldBe` "theater"
      it "defence → defense"        $ Tui.normMeaning "defence"      `shouldBe` "defense"
      it "licence → license"        $ Tui.normMeaning "licence"      `shouldBe` "license"
      it "analyse → analyze"        $ Tui.normMeaning "analyse"      `shouldBe` "analyze"
      it "organise → organize"      $ Tui.normMeaning "organise"     `shouldBe` "organize"
      it "organisation → organization" $ Tui.normMeaning "organisation" `shouldBe` "organization"
      it "catalogue → catalog"      $ Tui.normMeaning "catalogue"    `shouldBe` "catalog"
      it "neighbourhood → neighborhood" $ Tui.normMeaning "neighbourhood" `shouldBe` "neighborhood"
      it "four unchanged"           $ Tui.normMeaning "four"         `shouldBe` "four"
      it "rise unchanged"           $ Tui.normMeaning "rise"         `shouldBe` "rise"
      it "surprise unchanged"       $ Tui.normMeaning "surprise"     `shouldBe` "surprise"

  describe "normReading" $ do
    it "passes through hiragana unchanged" $
      Tui.normReading "にち" `shouldBe` "にち"
    it "converts romaji to hiragana" $
      Tui.normReading "nichi" `shouldBe` "にち"
    it "lowercases before converting" $
      Tui.normReading "NICHI" `shouldBe` "にち"
    it "trims whitespace" $
      Tui.normReading "  にち  " `shouldBe` "にち"
    it "romaji with doubled consonant" $
      Tui.normReading "gakkou" `shouldBe` "がっこう"
    it "romaji with apostrophe (n')" $
      Tui.normReading "n'a" `shouldBe` "んあ"

  -- This used to be an 'error' in the NoOverlay branch, guarded only by the
  -- caller. An exception here escapes customMain and skips the end-of-session
  -- History.saveHistory, so it is now total.
  describe "overlayViewport" $ do
    it "maps the all-info overlay to its viewport" $
      Tui.overlayViewport Tui.AllInfo `shouldBe` Just Tui.InfoViewport
    it "maps the user overlay to its viewport" $
      Tui.overlayViewport Tui.UserInfo `shouldBe` Just Tui.UserViewport
    it "maps the review-schedule overlay to its viewport" $
      Tui.overlayViewport Tui.ReviewSchedule `shouldBe` Just Tui.ReviewViewport
    it "has no viewport when no overlay is open, rather than crashing" $
      Tui.overlayViewport Tui.NoOverlay `shouldBe` Nothing

  -- Quitting mid-session leaves the reviews unrecorded on WaniKani, so the
  -- same items return next run. Persisting their misses anyway counted one
  -- mistake once per abandoned attempt.
  describe "recordableWrongCounts" $ do
    let progWrong = M.fromList [ (sid 1, Tui.Progress True True False 2 1) ]
        stAfter   = stateWith progWrong M.empty

    it "records this session's misses once the batch was submitted" $
      Tui.recordableWrongCounts stAfter { Tui.stSubmitAttempted = True }
        `shouldBe` Just [(sid 1, 2, 1)]

    it "records nothing (Nothing) when abandoned before submitting" $
      Tui.recordableWrongCounts stAfter { Tui.stSubmitAttempted = False }
        `shouldBe` Nothing

    it "still reports the raw session counts regardless of submission" $
      Tui.sessionWrongCounts stAfter { Tui.stSubmitAttempted = False }
        `shouldBe` [(sid 1, 2, 1)]

    -- Distinct from abandonment: a completed round with no misses is
    -- 'Just []', which still lets applyPracticeSession graduate its subjects.
    it "records an empty (Just []) completed round when nothing was missed" $
      Tui.recordableWrongCounts
        (stateWith (M.fromList [ (sid 1, Tui.Progress True True True 0 0) ]) M.empty)
          { Tui.stSubmitAttempted = True } `shouldBe` Just []

  -- Validation for adding a meaning synonym: catch the common mistakes
  -- client-side before any network call, and return the complete new list
  -- WaniKani should store (it replaces the array wholesale).
  describe "mergeSynonym" $ do
    it "appends a new synonym to the existing list" $
      Tui.mergeSynonym ["evade"] ["sun", "day", "evade"] "flee"
        `shouldBe` Right ["evade", "flee"]
    it "trims surrounding whitespace" $
      Tui.mergeSynonym [] [] "  flee  " `shouldBe` Right ["flee"]
    it "rejects an empty synonym" $
      Tui.mergeSynonym [] [] "   " `shouldBe` Left "Enter a synonym."
    it "rejects a too-long synonym" $
      Tui.mergeSynonym [] [] (T.replicate (Tui.maxSynonymLength + 1) "x")
        `shouldBe` Left "Too long (max 64 characters)."
    it "rejects a duplicate existing synonym (case-insensitive)" $
      Tui.mergeSynonym ["Flee"] ["Flee"] "flee"
        `shouldBe` Left "Already accepted for this item."
    it "rejects a duplicate of an already-accepted meaning" $
      Tui.mergeSynonym [] ["Sun"] "sun"
        `shouldBe` Left "Already accepted for this item."
    it "rejects adding beyond the synonym cap" $
      Tui.mergeSynonym (map (T.pack . show) [1 .. Tui.maxSynonyms]) [] "extra"
        `shouldBe` Left "WaniKani allows at most 8 synonyms."

  describe "checkAnswer" $ do

    describe "meaning questions" $ do
      it "accepts exact match" $
        fst (Tui.checkAnswer (mkQ kanjiSubj Tui.QMeaning) "Sun") `shouldBe` True
      it "accepts case-insensitive match" $
        fst (Tui.checkAnswer (mkQ kanjiSubj Tui.QMeaning) "sun") `shouldBe` True
      it "accepts second accepted meaning" $
        fst (Tui.checkAnswer (mkQ kanjiSubj Tui.QMeaning) "day") `shouldBe` True
      it "rejects wrong answer" $
        fst (Tui.checkAnswer (mkQ kanjiSubj Tui.QMeaning) "Moon") `shouldBe` False
      it "returns accepted meanings on failure" $
        snd (Tui.checkAnswer (mkQ kanjiSubj Tui.QMeaning) "Moon") `shouldBe` ["Sun", "Day"]

    -- kroki must accept exactly what WaniKani accepts. Checking only the
    -- primary meanings marked a legitimate answer wrong, requeued it,
    -- recorded it as a leech, and submitted it to WaniKani as incorrect --
    -- lowering the SRS stage for an answer WaniKani would have taken.
    describe "meanings WaniKani accepts beyond the primary ones" $ do
      let auxSubj = kanjiSubj
            { Api.subjAuxWhitelist = ["Evade"]
            , Api.subjAuxBlacklist = ["Escape"]
            }
          synSubj = kanjiSubj { Api.subjUserSynonyms = ["daylight"] }

      it "accepts a whitelisted auxiliary meaning" $
        fst (Tui.checkAnswer (mkQ auxSubj Tui.QMeaning) "Evade") `shouldBe` True
      it "accepts a whitelisted auxiliary meaning case-insensitively" $
        fst (Tui.checkAnswer (mkQ auxSubj Tui.QMeaning) "evade") `shouldBe` True
      it "accepts one of the user's own synonyms" $
        fst (Tui.checkAnswer (mkQ synSubj Tui.QMeaning) "Daylight") `shouldBe` True
      it "still rejects a genuinely wrong answer" $
        fst (Tui.checkAnswer (mkQ auxSubj Tui.QMeaning) "Moon") `shouldBe` False
      it "rejects a blacklisted meaning" $
        fst (Tui.checkAnswer (mkQ auxSubj Tui.QMeaning) "Escape") `shouldBe` False
      it "keeps displaying only the canonical meanings" $
        snd (Tui.checkAnswer (mkQ auxSubj Tui.QMeaning) "Moon") `shouldBe` ["Sun", "Day"]

      describe "acceptedMeanings" $ do
        it "unions primary, whitelist and user synonyms" $
          Tui.acceptedMeanings kanjiSubj
            { Api.subjAuxWhitelist = ["Evade"]
            , Api.subjUserSynonyms = ["daylight"]
            } `shouldBe` ["Sun", "Day", "Evade", "daylight"]
        it "excludes blacklisted meanings even if otherwise present" $
          Tui.acceptedMeanings kanjiSubj
            { Api.subjAuxWhitelist = ["Evade", "Escape"]
            , Api.subjAuxBlacklist = ["Escape"]
            } `shouldBe` ["Sun", "Day", "Evade"]
        it "is just the primary meanings when nothing extra is set" $
          Tui.acceptedMeanings kanjiSubj `shouldBe` ["Sun", "Day"]

    describe "reading questions" $ do
      it "accepts hiragana directly" $
        fst (Tui.checkAnswer (mkQ kanjiSubj Tui.QReading) "にち") `shouldBe` True
      it "accepts romaji (converted to hiragana)" $
        fst (Tui.checkAnswer (mkQ kanjiSubj Tui.QReading) "nichi") `shouldBe` True
      it "rejects wrong reading" $
        fst (Tui.checkAnswer (mkQ kanjiSubj Tui.QReading) "ka") `shouldBe` False
      it "returns accepted readings on failure" $
        snd (Tui.checkAnswer (mkQ kanjiSubj Tui.QReading) "ka") `shouldBe` ["にち", "じつ"]
      it "accepts vocab reading" $
        fst (Tui.checkAnswer (mkQ vocabSubj Tui.QReading) "gakkou") `shouldBe` True

    describe "british spelling normalisation" $ do
      let britSubj = kanjiSubj { Api.subjMeanings = ["Color"] }
      it "user types british, accepted is american" $
        fst (Tui.checkAnswer (mkQ britSubj Tui.QMeaning) "colour") `shouldBe` True
      let britSubj2 = kanjiSubj { Api.subjMeanings = ["Colour"] }
      it "user types american, accepted is british" $
        fst (Tui.checkAnswer (mkQ britSubj2 Tui.QMeaning) "color") `shouldBe` True

    describe "radical (meaning only)" $ do
      it "accepts radical meaning" $
        fst (Tui.checkAnswer (mkQ radicalSubj Tui.QMeaning) "one") `shouldBe` True
      it "rejects wrong meaning" $
        fst (Tui.checkAnswer (mkQ radicalSubj Tui.QMeaning) "two") `shouldBe` False

    describe "empty input" $ do
      it "rejects empty meaning" $
        fst (Tui.checkAnswer (mkQ kanjiSubj Tui.QMeaning) "") `shouldBe` False
      it "rejects empty reading" $
        fst (Tui.checkAnswer (mkQ kanjiSubj Tui.QReading) "") `shouldBe` False
      it "rejects whitespace-only meaning" $
        fst (Tui.checkAnswer (mkQ kanjiSubj Tui.QMeaning) "   ") `shouldBe` False

  describe "requeueAfterK" $ do
    let qs = map (\n -> mkQ kanjiSubj { Api.subjId = sid n } Tui.QMeaning) [1..5]
        q0 = mkQ kanjiSubj Tui.QMeaning

    it "inserts at position k" $
      map (Api.subjId . Tui.qSubject) (Tui.requeueAfterK 2 q0 qs)
        `shouldBe` map sid [1, 2, 1, 3, 4, 5]
    it "k=0 puts item at front" $
      Api.subjId (Tui.qSubject (head (Tui.requeueAfterK 0 q0 qs)))
        `shouldBe` Api.subjId kanjiSubj
    it "k > length appends at end" $
      last (Tui.requeueAfterK 100 q0 qs) `shouldBe` q0
    it "empty queue returns singleton" $
      Tui.requeueAfterK 3 q0 [] `shouldBe` [q0]
    it "negative k treated as 0 (front)" $
      Api.subjId (Tui.qSubject (head (Tui.requeueAfterK (-1) q0 qs)))
        `shouldBe` Api.subjId kanjiSubj

  describe "requeueOnly" $ do
    let q0   = mkQ kanjiSubj Tui.QMeaning
        qs   = map (\n -> mkQ (kanjiSubj { Api.subjId = sid n }) Tui.QMeaning) [2..4]
        st0  = (stateWith (M.singleton (sid 1) (Tui.initProgress kanjiSubj)) M.empty)
                 { Tui.stQueue        = q0 : qs
                 , Tui.stQueueWidget  = L.list Tui.QueueList (Vec.fromList (q0 : qs)) 1
                 , Tui.stRequeueAfter = 2
                 , Tui.stWrong        = 0
                 }

    it "does not increment wrong count" $ do
      let st' = Tui.requeueOnly q0 st0
      Tui.stWrong st' `shouldBe` 0

    it "does not increment pMeaningWrong" $ do
      let st' = Tui.requeueOnly q0 st0
      Tui.pMeaningWrong (Tui.stProgress st' M.! sid 1) `shouldBe` 0

    it "removes item from front of queue" $ do
      let st' = Tui.requeueOnly q0 st0
      Api.subjId (Tui.qSubject (head (Tui.stQueue st'))) `shouldBe` sid 2

    it "reinserts item at requeue position" $ do
      let st' = Tui.requeueOnly q0 st0
      map (Api.subjId . Tui.qSubject) (Tui.stQueue st') `shouldBe` map sid [2, 3, 1, 4]

    it "clears input" $ do
      let st' = Tui.requeueOnly q0 st0 { Tui.stInput = "foo" }
      Tui.stInput st' `shouldBe` ""

  describe "initProgress" $ do
    it "kanji needs reading" $
      Tui.pReadingNeeded (Tui.initProgress kanjiSubj) `shouldBe` True
    it "radical does not need reading" $
      Tui.pReadingNeeded (Tui.initProgress radicalSubj) `shouldBe` False
    it "vocab with readings needs reading" $
      Tui.pReadingNeeded (Tui.initProgress vocabSubj) `shouldBe` True
    it "kanji with no readings does not need reading" $
      Tui.pReadingNeeded (Tui.initProgress (kanjiSubj { Api.subjReadings = [] })) `shouldBe` False
    it "starts with nothing correct" $ do
      let p = Tui.initProgress kanjiSubj
      Tui.pMeaningOk p `shouldBe` False
      Tui.pReadingOk p `shouldBe` False
    it "starts with zero wrong counts" $ do
      let p = Tui.initProgress kanjiSubj
      Tui.pMeaningWrong p `shouldBe` 0
      Tui.pReadingWrong p `shouldBe` 0

  describe "markOk" $ do
    let prog0 = M.singleton (sid 1) (Tui.initProgress kanjiSubj)

    it "marks meaning ok" $ do
      let result = Tui.markOk kanjiSubj Tui.QMeaning prog0
      Tui.pMeaningOk (result M.! sid 1) `shouldBe` True

    it "marks reading ok" $ do
      let result = Tui.markOk kanjiSubj Tui.QReading prog0
      Tui.pReadingOk (result M.! sid 1) `shouldBe` True

    it "marking meaning ok does not affect readingOk" $ do
      let result = Tui.markOk kanjiSubj Tui.QMeaning prog0
      Tui.pReadingOk (result M.! sid 1) `shouldBe` False

    it "marking reading ok does not affect meaningOk" $ do
      let result = Tui.markOk kanjiSubj Tui.QReading prog0
      Tui.pMeaningOk (result M.! sid 1) `shouldBe` False

    it "does not affect other subjects" $
      M.lookup (sid 99) (Tui.markOk kanjiSubj Tui.QMeaning prog0) `shouldBe` Nothing

  describe "incWrong" $ do
    let prog0 = M.singleton (sid 1) (Tui.initProgress kanjiSubj)

    it "increments meaning wrong count" $
      Tui.pMeaningWrong (Tui.incWrong kanjiSubj Tui.QMeaning prog0 M.! sid 1) `shouldBe` 1
    it "increments reading wrong count" $
      Tui.pReadingWrong (Tui.incWrong kanjiSubj Tui.QReading prog0 M.! sid 1) `shouldBe` 1
    it "meaning wrong does not affect reading wrong" $
      Tui.pReadingWrong (Tui.incWrong kanjiSubj Tui.QMeaning prog0 M.! sid 1) `shouldBe` 0
    it "reading wrong does not affect meaning wrong" $
      Tui.pMeaningWrong (Tui.incWrong kanjiSubj Tui.QReading prog0 M.! sid 1) `shouldBe` 0
    it "accumulates multiple wrongs" $ do
      let result = ( Tui.incWrong kanjiSubj Tui.QMeaning
                   . Tui.incWrong kanjiSubj Tui.QMeaning
                   $ prog0
                   ) M.! sid 1
      Tui.pMeaningWrong result `shouldBe` 2

  describe "mkSubmissions" $ do
    let mkAsg s a = Api.Assignment { Api.asId = a, Api.asSubjectId = s, Api.asSrsStage = Api.Apprentice }
        subjToAsg = M.fromList [(sid 1, mkAsg (sid 1) (aid 101)), (sid 3, mkAsg (sid 3) (aid 303))]

    it "produces one submission per subject with an assignment" $ do
      let prog = M.fromList
            [ (sid 1, (Tui.initProgress kanjiSubj) { Tui.pMeaningWrong = 0, Tui.pReadingWrong = 1 })
            , (sid 3, (Tui.initProgress vocabSubj)  { Tui.pMeaningWrong = 2, Tui.pReadingWrong = 0 })
            ]
          subs = Tui.mkSubmissions (stateWith prog subjToAsg)
      length subs `shouldBe` 2

    it "carries wrong counts into submission" $ do
      let prog = M.singleton (sid 1)
            (Tui.initProgress kanjiSubj) { Tui.pMeaningWrong = 3, Tui.pReadingWrong = 1 }
          [sub] = Tui.mkSubmissions (stateWith prog subjToAsg)
      Tui.subWrongMeaning sub `shouldBe` 3
      Tui.subWrongReading sub `shouldBe` 1

    it "uses the correct assignment id" $ do
      let prog  = M.singleton (sid 1) (Tui.initProgress kanjiSubj)
          [sub] = Tui.mkSubmissions (stateWith prog subjToAsg)
      Tui.subAssignmentId sub `shouldBe` aid 101

    it "excludes subjects without an assignment mapping" $ do
      let prog = M.singleton (sid 99) (Tui.initProgress (kanjiSubj { Api.subjId = sid 99 }))
          subs = Tui.mkSubmissions (stateWith prog subjToAsg)
      subs `shouldBe` []

    it "includes subject with zero wrong counts" $ do
      let prog  = M.singleton (sid 1) (Tui.initProgress kanjiSubj)
          [sub] = Tui.mkSubmissions (stateWith prog subjToAsg)
      Tui.subWrongMeaning sub `shouldBe` 0
      Tui.subWrongReading sub `shouldBe` 0

  describe "sessionWrongCounts" $ do
    it "omits subjects with zero wrong counts" $ do
      let prog = M.singleton (sid 1) (Tui.initProgress kanjiSubj)
      Tui.sessionWrongCounts (stateWith prog M.empty) `shouldBe` []

    it "includes subjects missed at least once, with their counts" $ do
      let prog = M.singleton (sid 1)
            (Tui.initProgress kanjiSubj) { Tui.pMeaningWrong = 2, Tui.pReadingWrong = 1 }
      Tui.sessionWrongCounts (stateWith prog M.empty) `shouldBe` [(sid 1, 2, 1)]

    it "includes a subject missed on only one axis" $ do
      let prog = M.singleton (sid 1)
            (Tui.initProgress kanjiSubj) { Tui.pMeaningWrong = 0, Tui.pReadingWrong = 3 }
      Tui.sessionWrongCounts (stateWith prog M.empty) `shouldBe` [(sid 1, 0, 3)]

  describe "missedBeforeLabel" $ do
    it "is Nothing when never missed" $
      Tui.missedBeforeLabel (0, 0) `shouldBe` Nothing
    it "shows meaning-only misses" $
      Tui.missedBeforeLabel (2, 0) `shouldBe` Just "meaning ×2"
    it "shows reading-only misses" $
      Tui.missedBeforeLabel (0, 5) `shouldBe` Just "reading ×5"
    it "shows both when both are non-zero" $
      Tui.missedBeforeLabel (1, 1) `shouldBe` Just "meaning ×1, reading ×1"

  describe "shouldAutoplay / markAutoplayed" $ do
    let vocabWithAudio = vocabSubj { Api.subjAudioUrls = ["https://example.com/a.mp3"] }
        readingQ = mkQ vocabWithAudio Tui.QReading
        meaningQ = mkQ vocabWithAudio Tui.QMeaning
        autoplayReady = (stateWith M.empty M.empty)
          { Tui.stAudioAutoplay = True
          , Tui.stAudioPlayer   = Just "mpv --really-quiet"
          }

    it "is False when autoplay is disabled" $
      Tui.shouldAutoplay autoplayReady { Tui.stAudioAutoplay = False } readingQ
        `shouldBe` False

    it "is True for a fresh vocab reading question with audio configured" $
      Tui.shouldAutoplay autoplayReady readingQ `shouldBe` True

    it "is False for a meaning question" $
      Tui.shouldAutoplay autoplayReady meaningQ `shouldBe` False

    it "is False for a kanji reading question" $
      Tui.shouldAutoplay autoplayReady (mkQ kanjiSubj Tui.QReading) `shouldBe` False

    it "is False without an audio player configured" $
      Tui.shouldAutoplay autoplayReady { Tui.stAudioPlayer = Nothing } readingQ
        `shouldBe` False

    it "is False once already marked auto-played" $
      let st' = Tui.markAutoplayed readingQ autoplayReady
      in Tui.shouldAutoplay st' readingQ `shouldBe` False
