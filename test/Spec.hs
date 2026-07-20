{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map.Strict as M
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)
import qualified Romaji
import qualified TuiSpec
import qualified Config
import qualified History
import qualified Api

main :: IO ()
main = hspec $ do
  TuiSpec.spec
  configSpec
  apiSpec
  historySpec

  describe "romajiToHiragana" $ do

    describe "basic vowels" $ do
      it "a → あ" $ Romaji.romajiToHiragana "a"  `shouldBe` "あ"
      it "i → い" $ Romaji.romajiToHiragana "i"  `shouldBe` "い"
      it "u → う" $ Romaji.romajiToHiragana "u"  `shouldBe` "う"
      it "e → え" $ Romaji.romajiToHiragana "e"  `shouldBe` "え"
      it "o → お" $ Romaji.romajiToHiragana "o"  `shouldBe` "お"

    describe "basic consonant+vowel" $ do
      it "ka → か" $ Romaji.romajiToHiragana "ka" `shouldBe` "か"
      it "ki → き" $ Romaji.romajiToHiragana "ki" `shouldBe` "き"
      it "sa → さ" $ Romaji.romajiToHiragana "sa" `shouldBe` "さ"
      it "ta → た" $ Romaji.romajiToHiragana "ta" `shouldBe` "た"
      it "na → な" $ Romaji.romajiToHiragana "na" `shouldBe` "な"
      it "ha → は" $ Romaji.romajiToHiragana "ha" `shouldBe` "は"
      it "ma → ま" $ Romaji.romajiToHiragana "ma" `shouldBe` "ま"
      it "ya → や" $ Romaji.romajiToHiragana "ya" `shouldBe` "や"
      it "ra → ら" $ Romaji.romajiToHiragana "ra" `shouldBe` "ら"
      it "wa → わ" $ Romaji.romajiToHiragana "wa" `shouldBe` "わ"

    describe "special spellings" $ do
      it "shi → し" $ Romaji.romajiToHiragana "shi" `shouldBe` "し"
      it "chi → ち" $ Romaji.romajiToHiragana "chi" `shouldBe` "ち"
      it "tsu → つ" $ Romaji.romajiToHiragana "tsu" `shouldBe` "つ"
      it "fu  → ふ" $ Romaji.romajiToHiragana "fu"  `shouldBe` "ふ"
      it "ji  → じ" $ Romaji.romajiToHiragana "ji"  `shouldBe` "じ"

    describe "palatalized sounds" $ do
      it "kya → きゃ" $ Romaji.romajiToHiragana "kya" `shouldBe` "きゃ"
      it "sha → しゃ" $ Romaji.romajiToHiragana "sha" `shouldBe` "しゃ"
      it "shu → しゅ" $ Romaji.romajiToHiragana "shu" `shouldBe` "しゅ"
      it "sho → しょ" $ Romaji.romajiToHiragana "sho" `shouldBe` "しょ"
      it "cha → ちゃ" $ Romaji.romajiToHiragana "cha" `shouldBe` "ちゃ"
      it "ryu → りゅ" $ Romaji.romajiToHiragana "ryu" `shouldBe` "りゅ"

    describe "ん (n)" $ do
      it "n' → ん"              $ Romaji.romajiToHiragana "n'"     `shouldBe` "ん"
      it "nn → ん (not んん)"   $ Romaji.romajiToHiragana "nn"     `shouldBe` "ん"
      it "trailing n → ん"      $ Romaji.romajiToHiragana "n"      `shouldBe` "ん"
      it "n before consonant"   $ Romaji.romajiToHiragana "nka"    `shouldBe` "んか"
      it "nn before vowel (nna)"$ Romaji.romajiToHiragana "nna"    `shouldBe` "んな"
      it "n before vowel stays" $ Romaji.romajiToHiragana "na"     `shouldBe` "な"
      it "kanna → かんな"       $ Romaji.romajiToHiragana "kanna"  `shouldBe` "かんな"
      it "denwa → でんわ"       $ Romaji.romajiToHiragana "denwa"  `shouldBe` "でんわ"
      it "n'a → んあ"           $ Romaji.romajiToHiragana "n'a"    `shouldBe` "んあ"
      it "shinnsei → しんせい"   $ Romaji.romajiToHiragana "shinnsei"    `shouldBe` "しんせい"

    describe "っ (small tsu / doubled consonant)" $ do
      it "kka → っか" $ Romaji.romajiToHiragana "kka"  `shouldBe` "っか"
      it "tte → って" $ Romaji.romajiToHiragana "tte"  `shouldBe` "って"
      it "ssh → っし" $ Romaji.romajiToHiragana "sshi" `shouldBe` "っし"
      it "pp  → っぱ" $ Romaji.romajiToHiragana "ppa"  `shouldBe` "っぱ"

    describe "multi-syllable words" $ do
      it "nihon → にほん"    $ Romaji.romajiToHiragana "nihon"    `shouldBe` "にほん"
      it "sakura → さくら"  $ Romaji.romajiToHiragana "sakura"   `shouldBe` "さくら"
      it "gakkou → がっこう" $ Romaji.romajiToHiragana "gakkou"  `shouldBe` "がっこう"
      it "macchi → まっち"  $ Romaji.romajiToHiragana "macchi"   `shouldBe` "まっち"
      it "chidimaru → ちぢまる"  $ Romaji.romajiToHiragana "chidimaru" `shouldBe` "ちぢまる"

    describe "case insensitivity" $ do
      it "KA → か" $ Romaji.romajiToHiragana "KA"  `shouldBe` "か"
      it "SHI → し" $ Romaji.romajiToHiragana "SHI" `shouldBe` "し"

  describe "romajiToHiraganaLive" $ do

    describe "complete input converts fully" $ do
      it "ka → か"  $ Romaji.romajiToHiraganaLive "ka"  `shouldBe` "か"
      it "shi → し" $ Romaji.romajiToHiraganaLive "shi" `shouldBe` "し"

    describe "pending suffix shown as-is" $ do
      it "k stays pending"  $ Romaji.romajiToHiraganaLive "k"  `shouldBe` "k"
      it "sh stays pending" $ Romaji.romajiToHiraganaLive "sh" `shouldBe` "sh"
      it "n stays pending"  $ Romaji.romajiToHiraganaLive "n"  `shouldBe` "n"

    describe "mixed converted + pending" $ do
      it "kak → か + k pending" $ Romaji.romajiToHiraganaLive "kak"  `shouldBe` "かk"
      it "kas → か + s pending" $ Romaji.romajiToHiraganaLive "kas"  `shouldBe` "かs"
      it "shan → しゃ + n pending" $ Romaji.romajiToHiraganaLive "shan" `shouldBe` "しゃn"
      it "shik → し + k pending"  $ Romaji.romajiToHiraganaLive "shik" `shouldBe` "しk"

    describe "nn handling" $ do
      it "nn alone → ん"   $ Romaji.romajiToHiraganaLive "nn"   `shouldBe` "ん"
      it "nna → んな"      $ Romaji.romajiToHiraganaLive "nna"  `shouldBe` "んな"
      it "kanna → かんな"  $ Romaji.romajiToHiraganaLive "kanna" `shouldBe` "かんな"

    describe "っ (doubled consonant)" $ do
      it "kka → っか" $ Romaji.romajiToHiraganaLive "kka" `shouldBe` "っか"
      it "kk pending" $ Romaji.romajiToHiraganaLive "kk"  `shouldBe` "っk"

--------------------------------------------------------------------------------
-- Config parsing tests
--------------------------------------------------------------------------------

configSpec :: Spec
configSpec = describe "parseConfig" $ do

  it "parses token" $
    Config.cfgToken (Config.parseConfig "token=abc123") `shouldBe` Just "abc123"

  it "parses batch_size" $
    Config.cfgBatchSize (Config.parseConfig "batch_size=5") `shouldBe` Just 5

  it "parses requeue_after" $
    Config.cfgRequeueAfter (Config.parseConfig "requeue_after=3") `shouldBe` Just 3

  it "parses audio_player with spaces in command" $
    Config.cfgAudioPlayer (Config.parseConfig "audio_player=mpv --really-quiet")
      `shouldBe` Just "mpv --really-quiet"

  it "ignores comment lines" $
    Config.cfgToken (Config.parseConfig "# this is a comment\ntoken=xyz") `shouldBe` Just "xyz"

  it "ignores blank lines" $
    Config.cfgToken (Config.parseConfig "\n\ntoken=abc\n\n") `shouldBe` Just "abc"

  it "returns Nothing for missing key" $
    Config.cfgToken (Config.parseConfig "") `shouldBe` Nothing

  it "returns Nothing for empty value" $
    Config.cfgToken (Config.parseConfig "token=") `shouldBe` Nothing

  it "returns Nothing for malformed int" $
    Config.cfgBatchSize (Config.parseConfig "batch_size=not_a_number") `shouldBe` Nothing

  it "trims whitespace around key and value" $
    Config.cfgToken (Config.parseConfig "  token  =  mytoken  ") `shouldBe` Just "mytoken"

  it "uses first occurrence when key appears twice" $
    Config.cfgToken (Config.parseConfig "token=first\ntoken=second") `shouldBe` Just "first"

  it "defaultBatchSize is 10" $
    Config.defaultBatchSize `shouldBe` 10

  it "defaultRequeueAfter is 7" $
    Config.defaultRequeueAfter `shouldBe` 7

  describe "audio_autoplay" $ do
    it "parses true" $
      Config.cfgAudioAutoplay (Config.parseConfig "audio_autoplay=true") `shouldBe` Just True
    it "parses false" $
      Config.cfgAudioAutoplay (Config.parseConfig "audio_autoplay=false") `shouldBe` Just False
    it "parses yes/no" $ do
      Config.cfgAudioAutoplay (Config.parseConfig "audio_autoplay=yes") `shouldBe` Just True
      Config.cfgAudioAutoplay (Config.parseConfig "audio_autoplay=no")  `shouldBe` Just False
    it "parses 1/0" $ do
      Config.cfgAudioAutoplay (Config.parseConfig "audio_autoplay=1") `shouldBe` Just True
      Config.cfgAudioAutoplay (Config.parseConfig "audio_autoplay=0") `shouldBe` Just False
    it "is case-insensitive" $
      Config.cfgAudioAutoplay (Config.parseConfig "audio_autoplay=TRUE") `shouldBe` Just True
    it "is Nothing when absent" $
      Config.cfgAudioAutoplay (Config.parseConfig "") `shouldBe` Nothing
    it "is Nothing for an unrecognized value" $
      Config.cfgAudioAutoplay (Config.parseConfig "audio_autoplay=maybe") `shouldBe` Nothing

--------------------------------------------------------------------------------
-- API JSON parsing tests
--------------------------------------------------------------------------------

apiSpec :: Spec
apiSpec = describe "Api JSON parsing" $ do

  describe "User" $ do
    let innerJson :: ByteString
        innerJson = "{\"username\":\"bob\",\"level\":5,\"profile_url\":\"https://example.com\"}"
    let envelopeJson :: ByteString
        envelopeJson = "{\"data\":{\"username\":\"bob\",\"level\":5,\"profile_url\":\"https://example.com\"}}"

    it "parses username" $
      fmap Api.userUsername (decode innerJson :: Maybe Api.User) `shouldBe` Just "bob"

    it "parses level" $
      fmap Api.userLevel (decode innerJson :: Maybe Api.User) `shouldBe` Just 5

    it "parses profile_url" $
      fmap Api.userProfileUrl (decode innerJson :: Maybe Api.User) `shouldBe` Just "https://example.com"

    it "fails on missing required field" $
      (decode "{\"username\":\"bob\"}" :: Maybe Api.User) `shouldBe` Nothing

    it "parses full API envelope via UserEnvelope" $
      fmap (Api.userUsername . Api.ueData) (decode envelopeJson :: Maybe Api.UserEnvelope)
        `shouldBe` Just "bob"

    it "UserEnvelope fails on missing data wrapper" $
      (decode innerJson :: Maybe Api.UserEnvelope) `shouldBe` Nothing

  describe "ReviewBucket" $ do
    let validJson :: ByteString
        validJson = "{\"available_at\":\"2024-01-01T00:00:00.000000Z\",\"subject_ids\":[1,2,3]}"

    it "parses subject_ids" $
      fmap Api.rbSubjectIds (decode validJson) `shouldBe` Just (map Api.SubjectId [1, 2, 3])

    it "fails on invalid available_at" $
      (decode "{\"available_at\":\"not-a-date\",\"subject_ids\":[]}" :: Maybe Api.ReviewBucket)
        `shouldBe` Nothing

  describe "Subject (kanji)" $ do
    -- WaniKani omits absent optional fields rather than sending null;
    -- aeson's .:? only yields Nothing for absent keys, not for null values
    -- when the target type is Text.
    let validJson :: ByteString
        validJson = mconcat
          [ "{\"id\":1,\"object\":\"kanji\",\"data\":{"
          , "\"level\":3,"
          , "\"characters\":\"\\u65e5\","
          , "\"meanings\":[{\"meaning\":\"Sun\",\"accepted_answer\":true}"
          , ",{\"meaning\":\"Day\",\"accepted_answer\":true}],"
          , "\"readings\":[{\"reading\":\"\\u306b\\u3061\",\"accepted_answer\":true}"
          , ",{\"reading\":\"\\u3058\\u3064\",\"accepted_answer\":false}],"
          , "\"visually_similar_subject_ids\":[440,449],"
          , "\"component_subject_ids\":[]}}"
          ]

    it "parses subject type" $
      fmap Api.subjType (decode validJson) `shouldBe` Just Api.Kanji

    it "parses accepted meanings only" $
      fmap Api.subjMeanings (decode validJson) `shouldBe` Just ["Sun", "Day"]

    it "parses visually similar subject ids" $
      fmap (map Api.unSubjectId . Api.subjVisuallySimilarIds) (decode validJson)
        `shouldBe` Just [440, 449]

    it "parses accepted readings only" $
      fmap Api.subjReadings (decode validJson) `shouldBe` Just ["\12395\12385"]

    it "parses characters" $
      fmap Api.subjChars (decode validJson) `shouldBe` Just (Just "\26085")

  describe "Subject (radical)" $ do
    let validJson :: ByteString
        validJson = mconcat
          [ "{\"id\":2,\"object\":\"radical\",\"data\":{"
          , "\"level\":1,"
          , "\"characters\":\"\\u4e00\","
          , "\"meanings\":[{\"meaning\":\"One\",\"accepted_answer\":true}],"
          , "\"component_subject_ids\":[]}}"
          ]

    it "has no readings" $
      fmap Api.subjReadings (decode validJson) `shouldBe` Just []

    it "has no reading mnemonic" $
      fmap Api.subjReadingMnemonic (decode validJson) `shouldBe` Just Nothing

  describe "Subject (unknown type)" $ do
    it "fails to parse unknown object type" $
      (decode "{\"id\":1,\"object\":\"unknown\",\"data\":{}}" :: Maybe Api.Subject)
        `shouldBe` Nothing

--------------------------------------------------------------------------------
-- Cross-session leech history tests
--------------------------------------------------------------------------------

historySpec :: Spec
historySpec = describe "History" $ do

  let t1 = UTCTime (fromGregorian 2026 7 1) (secondsToDiffTime 0)
      t2 = UTCTime (fromGregorian 2026 7 17) (secondsToDiffTime 0)

  describe "LeechEntry JSON" $ do
    let entry = History.LeechEntry
          { History.leSubjectId    = Api.SubjectId 42
          , History.leWrongMeaning = 2
          , History.leWrongReading = 1
          , History.leLastSeen     = t1
          , History.leRetired      = False
          , History.leRelapses     = 0
          }

    it "round-trips through encode/decode" $
      (decode (encode entry) :: Maybe History.LeechEntry) `shouldBe` Just entry

    it "decodes a fixed JSON object" $ do
      let json :: ByteString
          json = "{\"subject_id\":42,\"wrong_meaning\":2,\"wrong_reading\":1,\"last_seen\":\"2026-07-01T00:00:00Z\"}"
      fmap History.leSubjectId (decode json) `shouldBe` Just (Api.SubjectId 42)
      fmap History.leWrongMeaning (decode json) `shouldBe` Just 2
      fmap History.leWrongReading (decode json) `shouldBe` Just 1

    it "defaults retired/relapses to False/0 for a pre-existing file missing those keys" $ do
      let json :: ByteString
          json = "{\"subject_id\":42,\"wrong_meaning\":2,\"wrong_reading\":1,\"last_seen\":\"2026-07-01T00:00:00Z\"}"
      fmap History.leRetired (decode json) `shouldBe` Just False
      fmap History.leRelapses (decode json) `shouldBe` Just 0

  describe "mergeSession" $ do
    it "adds a new entry for a subject missed for the first time" $ do
      let merged = History.mergeSession t1 [(Api.SubjectId 1, 2, 0)] M.empty
      History.historyCounts merged `shouldBe` M.singleton (Api.SubjectId 1) (2, 0)

    it "accumulates counts across repeated merges for the same subject" $ do
      let after1 = History.mergeSession t1 [(Api.SubjectId 1, 1, 1)] M.empty
          after2 = History.mergeSession t2 [(Api.SubjectId 1, 2, 0)] after1
      History.historyCounts after2 `shouldBe` M.singleton (Api.SubjectId 1) (3, 1)

    it "bumps last_seen to the latest merge" $ do
      let after1 = History.mergeSession t1 [(Api.SubjectId 1, 1, 0)] M.empty
          after2 = History.mergeSession t2 [(Api.SubjectId 1, 1, 0)] after1
      fmap History.leLastSeen (M.lookup (Api.SubjectId 1) after2) `shouldBe` Just t2

    it "leaves untouched entries alone" $ do
      let existing = History.mergeSession t1 [(Api.SubjectId 1, 1, 0)] M.empty
          merged   = History.mergeSession t2 [(Api.SubjectId 2, 5, 0)] existing
      History.historyCounts merged `shouldBe` M.fromList
        [ (Api.SubjectId 1, (1, 0)), (Api.SubjectId 2, (5, 0)) ]

    it "un-retires and bumps leRelapses when a retired leech is wrong again in a real review" $ do
      let graduated = History.applyPracticeSession t1 [Api.SubjectId 1] []
                        (History.mergeSession t1 [(Api.SubjectId 1, 2, 0)] M.empty)
          relapsed  = History.mergeSession t2 [(Api.SubjectId 1, 1, 0)] graduated
          Just e    = M.lookup (Api.SubjectId 1) relapsed
      History.leRetired e `shouldBe` False
      History.leRelapses e `shouldBe` 1
      History.historyCounts relapsed `shouldBe` M.singleton (Api.SubjectId 1) (1, 0)

    it "gives a relapsed leech higher weight than a fresh leech with equal raw misses" $ do
      let graduated    = History.applyPracticeSession t1 [Api.SubjectId 1] []
                           (History.mergeSession t1 [(Api.SubjectId 1, 2, 0)] M.empty)
          relapsed     = History.mergeSession t2 [(Api.SubjectId 1, 1, 0)] graduated
          fresh        = History.mergeSession t2 [(Api.SubjectId 2, 1, 0)] M.empty
          Just relapsedE = M.lookup (Api.SubjectId 1) relapsed
          Just freshE    = M.lookup (Api.SubjectId 2) fresh
      (History.leechWeight relapsedE > History.leechWeight freshE) `shouldBe` True

  describe "historyCounts" $
    it "projects to (meaning, reading) pairs" $ do
      let merged = History.mergeSession t1 [(Api.SubjectId 1, 3, 4)] M.empty
      History.historyCounts merged `shouldBe` M.singleton (Api.SubjectId 1) (3, 4)

  describe "applyPracticeSession" $ do
    it "retires (rather than drops) a leech answered fully correctly this round" $ do
      let existing = History.mergeSession t1 [(Api.SubjectId 1, 2, 1)] M.empty
          after    = History.applyPracticeSession t2 [Api.SubjectId 1] [] existing
      -- record kept for relapse-tracking purposes, not deleted
      History.historyCounts after `shouldBe` M.singleton (Api.SubjectId 1) (2, 1)
      fmap History.leRetired (M.lookup (Api.SubjectId 1) after) `shouldBe` Just True
      -- but excluded from the active leech list
      M.null (History.activeLeeches after) `shouldBe` True

    it "resets (not adds) counts for a leech still missed this round" $ do
      let existing = History.mergeSession t1 [(Api.SubjectId 1, 5, 5)] M.empty
          after    = History.applyPracticeSession t2 [Api.SubjectId 1]
                       [(Api.SubjectId 1, 1, 0)] existing
      History.historyCounts after `shouldBe` M.singleton (Api.SubjectId 1) (1, 0)

    it "bumps last_seen for a leech that is kept" $ do
      let existing = History.mergeSession t1 [(Api.SubjectId 1, 1, 0)] M.empty
          after    = History.applyPracticeSession t2 [Api.SubjectId 1]
                       [(Api.SubjectId 1, 1, 0)] existing
      fmap History.leLastSeen (M.lookup (Api.SubjectId 1) after) `shouldBe` Just t2

    it "leaves subjects not part of this practice round untouched" $ do
      let existing = History.mergeSession t1
                       [(Api.SubjectId 1, 1, 0), (Api.SubjectId 2, 3, 0)] M.empty
          after    = History.applyPracticeSession t2 [Api.SubjectId 1] [] existing
      -- subject 1 is retired (record kept) but subject 2's entry is untouched
      History.historyCounts after `shouldBe` M.fromList
        [ (Api.SubjectId 1, (1, 0)), (Api.SubjectId 2, (3, 0)) ]
      History.historyCounts (History.activeLeeches after) `shouldBe`
        M.singleton (Api.SubjectId 2) (3, 0)
