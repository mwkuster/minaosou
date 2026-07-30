{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Control.Exception
  ( ErrorCall(..), SomeException, AsyncException(ThreadKilled)
  , bracket, throwIO, toException, try )
import Data.Aeson (decode, encode)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import Data.Char (isDigit)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time (UTCTime(..), addUTCTime, fromGregorian, getCurrentTime, secondsToDiffTime)
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.Internal as HCI
import qualified Network.HTTP.Req as Req
import Network.HTTP.Types (Status(..), http11)
import System.Directory
  ( createDirectoryIfMissing, doesFileExist, getTemporaryDirectory
  , removeDirectoryRecursive )
import System.FilePath ((</>))
import qualified Romaji
import qualified TuiSpec
import qualified Config
import qualified History
import qualified JsonStore
import qualified Util
import qualified Api

main :: IO ()
main = hspec $ do
  TuiSpec.spec
  configSpec
  apiSpec
  historySpec
  utilSpec
  jsonStoreSpec

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
      it "n before vowel stays" $ Romaji.romajiToHiragana "na"     `shouldBe` "な"
      it "denwa → でんわ"       $ Romaji.romajiToHiragana "denwa"  `shouldBe` "でんわ"
      it "n'a → んあ"           $ Romaji.romajiToHiragana "n'a"    `shouldBe` "んあ"
      it "shinnsei → しんせい"   $ Romaji.romajiToHiragana "shinnsei"    `shouldBe` "しんせい"

      -- wanakana / WaniKani convention: "nn" is a committed ん (both n's
      -- consumed), so a vowel after it stands alone rather than joining the
      -- second n into a な-row syllable.
      describe "nn consumes both n's (a following vowel stands alone)" $ do
        it "nna → んあ"            $ Romaji.romajiToHiragana "nna"    `shouldBe` "んあ"
        it "kanna → かんあ"        $ Romaji.romajiToHiragana "kanna"  `shouldBe` "かんあ"
        it "onna → おんあ"         $ Romaji.romajiToHiragana "onna"   `shouldBe` "おんあ"
        -- and the ways to actually get ん + な-row:
        it "on'na → おんな"        $ Romaji.romajiToHiragana "on'na"  `shouldBe` "おんな"
        it "onnna → おんな"        $ Romaji.romajiToHiragana "onnna"  `shouldBe` "おんな"
        -- 全員: "zennin" and "zen'in" now converge on ぜんいん
        it "zennin → ぜんいん"     $ Romaji.romajiToHiragana "zennin" `shouldBe` "ぜんいん"
        it "zen'in → ぜんいん"     $ Romaji.romajiToHiragana "zen'in" `shouldBe` "ぜんいん"

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
      it "nna → んあ"      $ Romaji.romajiToHiraganaLive "nna"  `shouldBe` "んあ"
      it "kanna → かんあ"  $ Romaji.romajiToHiraganaLive "kanna" `shouldBe` "かんあ"
      -- Live keeps a trailing lone "n" pending (the user might still type
      -- "na", "ni", …), so mid-type "zennin" shows the last n as-is. It
      -- resolves to ん on submission -- see the romajiToHiragana "zennin →
      -- ぜんいん" case above, which is what checkAnswer/normReading use.
      it "zennin → ぜんい + pending n" $
        Romaji.romajiToHiraganaLive "zennin" `shouldBe` "ぜんいn"

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

  describe "PagedEnvelope" $ do
    let withNextUrl :: ByteString
        withNextUrl =
          "{\"data\":[1,2,3],\"pages\":{\"next_url\":\"https://api.wanikani.com/v2/assignments?page_after_id=3\",\"per_page\":500}}"
        withoutNextUrl :: ByteString
        withoutNextUrl =
          "{\"data\":[1,2,3],\"pages\":{\"next_url\":null,\"per_page\":500}}"
        noPagesKey :: ByteString
        noPagesKey = "{\"data\":[1,2,3]}"

    it "parses data" $
      fmap Api.peData (decode withNextUrl :: Maybe (Api.PagedEnvelope Int))
        `shouldBe` Just [1, 2, 3]

    it "parses a present next_url" $
      fmap Api.peNextUrl (decode withNextUrl :: Maybe (Api.PagedEnvelope Int))
        `shouldBe` Just (Just "https://api.wanikani.com/v2/assignments?page_after_id=3")

    it "parses a null next_url as Nothing" $
      fmap Api.peNextUrl (decode withoutNextUrl :: Maybe (Api.PagedEnvelope Int))
        `shouldBe` Just Nothing

    it "defaults to Nothing when the pages key itself is absent" $
      fmap Api.peNextUrl (decode noPagesKey :: Maybe (Api.PagedEnvelope Int))
        `shouldBe` Just Nothing

  describe "admitRequest (rate limiting)" $ do
    let t0 = UTCTime (fromGregorian 2026 7 22) (secondsToDiffTime 0)
        at secs = addUTCTime (fromIntegral (secs :: Int)) t0

    it "admits a request when the window is empty" $
      Api.admitRequest 3 t0 [] `shouldBe` Right [t0]

    it "records the new request at the front of the window" $
      Api.admitRequest 3 (at 1) [t0] `shouldBe` Right [at 1, t0]

    it "admits right up to the budget" $
      Api.admitRequest 3 (at 2) [at 1, t0] `shouldBe` Right [at 2, at 1, t0]

    -- A burst up to the budget must not be throttled: a normal batch of 30
    -- submissions should still go out at full speed.
    it "refuses once the budget is used up inside the window" $
      Api.admitRequest 3 (at 2) [at 1, at 1, t0]
        `shouldBe` Left 58   -- wait until the oldest (t0) ages out at t0+60

    -- At t0+60 the two t0+1 entries are still inside the window (59s old)
    -- but t0 itself is exactly 60s old and has aged out, freeing one slot.
    it "admits again once an old request has aged out of the window" $
      Api.admitRequest 3 (at 60) [at 1, at 1, t0]
        `shouldBe` Right [at 60, at 1, at 1]

    it "treats an exactly-60s-old request as outside the window" $
      Api.admitRequest 1 (at 60) [t0] `shouldBe` Right [at 60]

    it "still counts a 59s-old request as inside the window" $
      Api.admitRequest 1 (at 59) [t0] `shouldBe` Left 1

    it "drops aged-out entries rather than letting the window grow" $
      Api.admitRequest 3 (at 120) [at 59, t0] `shouldBe` Right [at 120]

    it "keeps a real budget's worth of headroom under WaniKani's 60/min" $
      Api.requestBudgetPerMinute `shouldSatisfy` (< 60)

  describe "Subject auxiliary_meanings" $ do
    let auxJson :: ByteString
        auxJson = "{\"id\":440,\"object\":\"kanji\",\"data\":{\"level\":1,\"characters\":\"\\u907F\",\
                  \\"meanings\":[{\"meaning\":\"Dodge\",\"accepted_answer\":true},\
                  \{\"meaning\":\"Avoid\",\"accepted_answer\":true}],\
                  \\"auxiliary_meanings\":[{\"meaning\":\"Evade\",\"type\":\"whitelist\"},\
                  \{\"meaning\":\"Escape\",\"type\":\"blacklist\"}],\
                  \\"readings\":[{\"reading\":\"\\u3072\",\"accepted_answer\":true}]}}"
        noAuxJson :: ByteString
        noAuxJson = "{\"id\":1,\"object\":\"kanji\",\"data\":{\"level\":1,\"characters\":\"\\u4E00\",\
                    \\"meanings\":[{\"meaning\":\"One\",\"accepted_answer\":true}],\
                    \\"readings\":[{\"reading\":\"\\u3044\\u3061\",\"accepted_answer\":true}]}}"

    it "parses whitelisted auxiliary meanings" $
      fmap Api.subjAuxWhitelist (decode auxJson :: Maybe Api.Subject)
        `shouldBe` Just ["Evade"]

    it "parses blacklisted auxiliary meanings separately" $
      fmap Api.subjAuxBlacklist (decode auxJson :: Maybe Api.Subject)
        `shouldBe` Just ["Escape"]

    it "leaves the primary meanings untouched" $
      fmap Api.subjMeanings (decode auxJson :: Maybe Api.Subject)
        `shouldBe` Just ["Dodge", "Avoid"]

    it "defaults to empty when the key is absent" $
      fmap Api.subjAuxWhitelist (decode noAuxJson :: Maybe Api.Subject)
        `shouldBe` Just []

    it "starts with no user synonyms (filled in from study_materials)" $
      fmap Api.subjUserSynonyms (decode auxJson :: Maybe Api.Subject)
        `shouldBe` Just []

  describe "StudyMaterial" $ do
    let smJson :: ByteString
        smJson = "{\"id\":65231,\"object\":\"study_material\",\
                 \\"data\":{\"subject_id\":8693,\"meaning_synonyms\":[\"both directions\"]}}"
        smNoSyn :: ByteString
        smNoSyn = "{\"id\":1,\"object\":\"study_material\",\"data\":{\"subject_id\":42}}"

    it "parses the record id (needed to update the record)" $
      fmap Api.smId (decode smJson :: Maybe Api.StudyMaterial)
        `shouldBe` Just (Api.StudyMaterialId 65231)

    it "parses the subject id" $
      fmap Api.smSubjectId (decode smJson :: Maybe Api.StudyMaterial)
        `shouldBe` Just (Api.SubjectId 8693)

    it "parses meaning synonyms" $
      fmap Api.smMeaningSynonyms (decode smJson :: Maybe Api.StudyMaterial)
        `shouldBe` Just ["both directions"]

    it "defaults to no synonyms when the key is absent" $
      fmap Api.smMeaningSynonyms (decode smNoSyn :: Maybe Api.StudyMaterial)
        `shouldBe` Just []

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

    it "adds this round's misses to a leech still missed in practice" $ do
      let existing = History.mergeSession t1 [(Api.SubjectId 1, 5, 5)] M.empty
          after    = History.applyPracticeSession t2 [Api.SubjectId 1]
                       [(Api.SubjectId 1, 1, 0)] existing
      -- meaning miss raises only the meaning counter; reading is unchanged
      History.historyCounts after `shouldBe` M.singleton (Api.SubjectId 1) (6, 5)

    it "raises the reading counter for a reading miss in practice" $ do
      let existing = History.mergeSession t1 [(Api.SubjectId 1, 2, 3)] M.empty
          after    = History.applyPracticeSession t2 [Api.SubjectId 1]
                       [(Api.SubjectId 1, 0, 2)] existing
      History.historyCounts after `shouldBe` M.singleton (Api.SubjectId 1) (2, 5)

    it "keeps a repeatedly-failed leech active and climbing in weight" $ do
      let existing = History.mergeSession t1 [(Api.SubjectId 1, 1, 0)] M.empty
          Just before = M.lookup (Api.SubjectId 1) existing
          after    = History.applyPracticeSession t2 [Api.SubjectId 1]
                       [(Api.SubjectId 1, 2, 1)] existing
          Just afterE = M.lookup (Api.SubjectId 1) after
      History.leRetired afterE `shouldBe` False
      (History.leechWeight afterE > History.leechWeight before) `shouldBe` True

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

--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------

-- | A 'HC.StatusCodeException' carrying the given status and body.
statusCodeException :: Int -> BS.ByteString -> BS.ByteString -> HC.HttpExceptionContent
statusCodeException code msg body =
  HC.StatusCodeException
    (HCI.Response
       (Status code msg) http11 [] ()
       (HC.createCookieJar []) (HCI.ResponseClose (pure ()))
       HC.defaultRequest [])
    body

utilSpec :: Spec
utilSpec = describe "Util" $ do

  describe "shortErr" $ do
    -- Regression: HttpException's Show instance dumps the whole multi-line
    -- Request record *before* the content saying what actually went wrong,
    -- so summarising by first line rendered every network failure alike as
    -- the constant string "VanillaHttpException (HttpExceptionRequest
    -- Request {" -- making a rate limit indistinguishable from a dropped
    -- connection in both the Done screen and the logs.
    it "reports the failure content, not the request dump" $
      Util.shortErr
        (toException (HC.HttpExceptionRequest HC.defaultRequest HC.ConnectionTimeout))
        `shouldBe` "ConnectionTimeout"

    it "unwraps req's VanillaHttpException wrapper" $
      Util.shortErr
        (toException (Req.VanillaHttpException
          (HC.HttpExceptionRequest HC.defaultRequest HC.ResponseTimeout)))
        `shouldBe` "ResponseTimeout"

    it "surfaces the status code of a rate-limited response" $
      Util.shortErr
        (toException (HC.HttpExceptionRequest HC.defaultRequest
          (statusCodeException 429 "Too Many Requests" "slow down")))
        `shouldBe` "HTTP 429 Too Many Requests: slow down"

    it "distinguishes a 422 rejection from a 429 rate limit" $
      Util.shortErr
        (toException (HC.HttpExceptionRequest HC.defaultRequest
          (statusCodeException 422 "Unprocessable Entity" "")))
        `shouldBe` "HTTP 422 Unprocessable Entity"

    it "describes an invalid URL" $
      Util.shortErr (toException (HC.InvalidUrlException "htp://x" "unknown scheme"))
        `shouldBe` "invalid URL htp://x: unknown scheme"

    it "describes a malformed JSON response" $
      Util.shortErr (toException (Req.JsonHttpException "expected Object"))
        `shouldBe` "malformed JSON in response: expected Object"

    it "falls back to displayException for non-HTTP exceptions" $
      Util.shortErr (toException (ErrorCall "boom")) `shouldBe` "boom"

    it "never spans multiple lines" $
      Util.shortErr (toException (ErrorCall "one\ntwo\nthree"))
        `shouldBe` "one two three"

    it "truncates an over-long message to 200 characters" $
      length (Util.shortErr (toException (ErrorCall (replicate 500 'x'))))
        `shouldBe` 200

  describe "trySync" $ do
    it "returns the value when nothing throws" $ do
      r <- Util.trySync (pure (42 :: Int))
      case r of
        Right v -> v `shouldBe` 42
        Left e  -> expectationFailure ("unexpected exception: " <> show e)

    it "catches a synchronous exception" $ do
      r <- Util.trySync (throwIO (userError "boom") :: IO ())
      case r of
        Left e  -> show e `shouldContain` "boom"
        Right _ -> expectationFailure "expected the exception to be caught"

    -- mapConcurrently cancels its siblings as soon as one action throws;
    -- swallowing that cancellation would record it as an ordinary failed
    -- review and persist it for a retry never actually attempted.
    it "rethrows an asynchronous exception instead of swallowing it" $ do
      outcome <- try (Util.trySync (throwIO ThreadKilled :: IO ()))
                   :: IO (Either SomeException (Either SomeException ()))
      case outcome of
        Left _  -> pure ()
        Right _ -> expectationFailure "trySync swallowed an asynchronous exception"

  describe "displayWidth" $ do
    it "counts ASCII as one cell each"        $ Util.displayWidth "abc"  `shouldBe` 3
    it "counts a kanji as two cells"          $ Util.displayWidth "贅"    `shouldBe` 2
    it "counts kana as two cells each"        $ Util.displayWidth "ぜい"  `shouldBe` 4
    it "mixes ASCII and wide characters"      $ Util.displayWidth "a贅b"  `shouldBe` 4
    it "is zero for empty text"               $ Util.displayWidth ""     `shouldBe` 0

  describe "wrapTextWidth" $ do
    it "leaves a line that already fits verbatim (alignment spaces intact)" $
      Util.wrapTextWidth 40 "accepted:    ぜい" `shouldBe` ["accepted:    ぜい"]

    it "keeps existing newlines as hard breaks" $
      Util.wrapTextWidth 80 "line one\nline two" `shouldBe` ["line one", "line two"]

    -- The bug: naive (code-point) wrapping counts "ab ぜい" as length 5 and
    -- leaves it on one line, but it is 7 display cells wide and overflows a
    -- 6-wide pane. Display-width wrapping must break it earlier.
    it "wraps by display width, not code-point count" $
      Util.wrapTextWidth 6 "ab ぜい cd" `shouldBe` ["ab", "ぜい", "cd"]

    it "never emits a line wider than the limit (CJK-heavy prose)" $ do
      let mnemonic = "you eat them with a zeiber (ぜい), a German saber. "
                  <> "A fork is just too plain for these extravagant shellfish."
          limit    = 30
          lns      = Util.wrapTextWidth limit mnemonic
      all ((<= limit) . Util.displayWidth) lns `shouldBe` True
      -- and nothing is dropped: rejoining recovers every word
      T.words (T.unwords lns) `shouldBe` T.words mnemonic

--------------------------------------------------------------------------------
-- JsonStore
--------------------------------------------------------------------------------

withTempDir :: (FilePath -> IO a) -> IO a
withTempDir act = do
  base  <- getTemporaryDirectory
  stamp <- (filter isDigit . show) <$> getCurrentTime
  let dir = base </> ("minaosou-test-" <> stamp)
  bracket
    (createDirectoryIfMissing True dir >> pure dir)
    removeDirectoryRecursive
    act

jsonStoreSpec :: Spec
jsonStoreSpec = describe "JsonStore" $ do

  it "round-trips a saved value" $ withTempDir $ \dir -> do
    let path = dir </> "state.json"
    JsonStore.saveJsonFile path ([1, 2, 3] :: [Int])
    loaded <- JsonStore.loadJsonFile [] path
    loaded `shouldBe` ([1, 2, 3] :: [Int])

  -- Regression: loadJsonFile used to be a lazy BL.readFile whose handle was
  -- still open and unforced when saveJsonFile truncated the very same path
  -- for writing, so the value being encoded was served from the file *after*
  -- truncation. Merging a new entry into a non-empty file silently dropped
  -- data -- exactly the pending-review retry path.
  it "does not lose existing entries when merging into the same file" $
    withTempDir $ \dir -> do
      let path = dir </> "pending.json"
      JsonStore.saveJsonFile path ([1, 2] :: [Int])
      existing <- JsonStore.loadJsonFile [] path
      JsonStore.saveJsonFile path (existing <> ([3] :: [Int]))
      loaded <- JsonStore.loadJsonFile [] path
      loaded `shouldBe` ([1, 2, 3] :: [Int])

  it "falls back on a missing file" $ withTempDir $ \dir ->
    JsonStore.loadJsonFile [7 :: Int] (dir </> "nope.json") >>= (`shouldBe` [7])

  it "falls back on a corrupt file" $ withTempDir $ \dir -> do
    let path = dir </> "corrupt.json"
    writeFile path "{not json"
    JsonStore.loadJsonFile [7 :: Int] path >>= (`shouldBe` [7])

  it "leaves no temp file behind after a successful write" $ withTempDir $ \dir -> do
    let path = dir </> "state.json"
    JsonStore.saveJsonFile path ([1] :: [Int])
    doesFileExist (path <> ".tmp") >>= (`shouldBe` False)

  it "keeps the previous contents readable across a rewrite" $ withTempDir $ \dir -> do
    let path = dir </> "state.json"
    JsonStore.saveJsonFile path ([1, 2] :: [Int])
    JsonStore.saveJsonFile path ([3, 4] :: [Int])
    JsonStore.loadJsonFile [] path >>= (`shouldBe` ([3, 4] :: [Int]))

  it "creates missing parent directories" $ withTempDir $ \dir -> do
    let path = dir </> "nested" </> "deep" </> "state.json"
    JsonStore.saveJsonFile path ([1] :: [Int])
    JsonStore.loadJsonFile [] path >>= (`shouldBe` ([1] :: [Int]))
