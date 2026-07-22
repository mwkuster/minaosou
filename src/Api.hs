{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Api
  ( SubjectId(..)
  , AssignmentId(..)

  , User(..)
  , UserEnvelope(..)
  , getUser

  , Summary(..)
  , ReviewBucket(..)
  , getSummary
  , reviewsAvailableNow
  , nextReviewBucket
  , reviewsPerHourNext24

  , SrsStage(..)
  , srsStageLabel
  , Assignment(..)
  , getAvailableAssignments
  , getAssignmentsBySubjectIds
  , getStillAvailableAssignmentIds
  , PagedEnvelope(..)

  , SubjectType(..)
  , Subject(..)
  , getSubjectsByIds
  , ReviewResult(..)
  , createReview
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Object, object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Key as Key
import Data.List (sortOn)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime(..), addUTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)

import qualified Data.ByteString.Char8 as BS8
import Network.HTTP.Req
import qualified Network.HTTP.Client as HC
import Network.HTTP.Types (status429, status500, status502, status503, status504)
import Control.Exception (SomeException, fromException)
import Control.Retry (RetryPolicyM, RetryStatus, capDelay, exponentialBackoff, limitRetries)
import Text.URI (mkURI)

--------------------------------------------------------------------------------
-- IDs
--------------------------------------------------------------------------------

-- | WaniKani subject ID (radical, kanji, vocabulary, kana_vocabulary).
newtype SubjectId = SubjectId { unSubjectId :: Int }
  deriving (Eq, Ord)

-- | WaniKani assignment ID. Distinct from SubjectId so the type checker
-- catches accidental swaps when threading IDs through maps and API calls.
newtype AssignmentId = AssignmentId { unAssignmentId :: Int }
  deriving (Eq, Ord)

instance Show SubjectId    where show (SubjectId i)    = show i
instance Show AssignmentId where show (AssignmentId i) = show i

instance ToJSON SubjectId      where toJSON (SubjectId i)        = toJSON i
instance ToJSON AssignmentId   where toJSON (AssignmentId i)     = toJSON i

instance FromJSON SubjectId    where parseJSON v = SubjectId    <$> parseJSON v
instance FromJSON AssignmentId where parseJSON v = AssignmentId <$> parseJSON v

--------------------------------------------------------------------------------
-- Shared API options
--------------------------------------------------------------------------------

-- | Standard auth + revision headers, shared by all API calls.
apiOpts :: String -> Option scheme
apiOpts token =
  header "Authorization" ("Bearer " <> BS8.pack token)
  <> header "Wanikani-Revision" "20170710"

-- | 'defaultHttpConfig' does not retry anything on its own — both retry
-- judges default to @const (const False)@. Every WaniKani call in this
-- module uses this config instead: up to 4 retries, exponential backoff
-- starting at 0.5s and capped at 8s per attempt, retrying only on genuinely
-- transient failures (connection drops/timeouts, and 429/5xx responses) —
-- never on 4xx like an expired token, where retrying can't help.
apiRetryPolicy :: RetryPolicyM IO
apiRetryPolicy = capDelay 8000000 (exponentialBackoff 500000) <> limitRetries 4

apiRetryJudge :: RetryStatus -> HC.Response b -> Bool
apiRetryJudge _ resp =
  HC.responseStatus resp `elem`
    [status429, status500, status502, status503, status504]

apiRetryJudgeException :: RetryStatus -> SomeException -> Bool
apiRetryJudgeException _ e = case fromException e of
  Just (VanillaHttpException (HC.HttpExceptionRequest _ content)) -> case content of
    HC.ResponseTimeout        -> True
    HC.ConnectionTimeout      -> True
    HC.ConnectionFailure _    -> True
    HC.ConnectionClosed       -> True
    HC.NoResponseDataReceived -> True
    _                         -> False
  _ -> False

retryingHttpConfig :: HttpConfig
retryingHttpConfig = defaultHttpConfig
  { httpConfigRetryPolicy         = apiRetryPolicy
  , httpConfigRetryJudge          = apiRetryJudge
  , httpConfigRetryJudgeException = apiRetryJudgeException
  }

--------------------------------------------------------------------------------
-- User
--------------------------------------------------------------------------------

data User = User
  { userUsername   :: Text
  , userLevel      :: Int
  , userProfileUrl :: Text
  } deriving (Show, Eq)

instance FromJSON User where
  parseJSON = withObject "User" $ \o ->
    User <$> o .: "username" <*> o .: "level" <*> o .: "profile_url"

newtype UserEnvelope = UserEnvelope { ueData :: User } deriving (Show, Eq)

instance FromJSON UserEnvelope where
  parseJSON = withObject "UserEnvelope" $ \o ->
    UserEnvelope <$> o .: "data"

getUser :: String -> IO User
getUser token = runReq retryingHttpConfig $ do
  resp <- req
    GET
    (https "api.wanikani.com" /: "v2" /: "user")
    NoReqBody
    jsonResponse
    (apiOpts token)
  pure (ueData (responseBody resp :: UserEnvelope))

--------------------------------------------------------------------------------
-- Summary (reviews timeline)
--------------------------------------------------------------------------------

data Summary = Summary
  { summaryReviews :: [ReviewBucket]
  } deriving (Show, Eq)

data ReviewBucket = ReviewBucket
  { rbAvailableAt :: UTCTime
  , rbSubjectIds  :: [SubjectId]
  } deriving (Show, Eq)

newtype SummaryEnvelope = SummaryEnvelope { seData :: Summary } deriving (Show)

instance FromJSON SummaryEnvelope where
  parseJSON = withObject "SummaryEnvelope" $ \o ->
    SummaryEnvelope <$> o .: "data"

instance FromJSON Summary where
  parseJSON = withObject "Summary" $ \o ->
    Summary <$> o .: "reviews"

instance FromJSON ReviewBucket where
  parseJSON = withObject "ReviewBucket" $ \o -> do
    t <- o .: "available_at"
    at <- maybe (fail "invalid available_at") pure (iso8601ParseM t)
    ReviewBucket at <$> o .: "subject_ids"

getSummary :: String -> IO Summary
getSummary token = runReq retryingHttpConfig $ do
  resp <- req
    GET
    (https "api.wanikani.com" /: "v2" /: "summary")
    NoReqBody
    jsonResponse
    (apiOpts token)
  let env = responseBody resp :: SummaryEnvelope
  pure (seData env)

reviewsAvailableNow :: UTCTime -> Summary -> Int
reviewsAvailableNow now s =
  sum [ length (rbSubjectIds b)
      | b <- summaryReviews s
      , rbAvailableAt b <= now
      ]

nextReviewBucket :: UTCTime -> Summary -> Maybe (UTCTime, Int)
nextReviewBucket now s =
  case sortOn rbAvailableAt
        [ b | b <- summaryReviews s
            , rbAvailableAt b > now
            , not (null (rbSubjectIds b))
        ] of
    (b:_) -> Just (rbAvailableAt b, length (rbSubjectIds b))
    []    -> Nothing

openAt :: UTCTime -> Summary -> Int
openAt t s =
  sum [ length (rbSubjectIds b)
      | b <- summaryReviews s
      , rbAvailableAt b <= t
      ]

newInWindow :: UTCTime -> UTCTime -> Summary -> Int
newInWindow start end s =
  sum [ length (rbSubjectIds b)
      | b <- summaryReviews s
      , rbAvailableAt b >  start
      , rbAvailableAt b <= end
      ]

-- Row 0: (now, openNow, openNow). Rows 1..: per hour new + cumulative open from now.
reviewsPerHourNext24 :: UTCTime -> Summary -> [(UTCTime, Int, Int)]
reviewsPerHourNext24 now s =
  let openNow = openAt now s

      mk :: Int -> (UTCTime, Int, Int)
      mk 0 = (now, openNow, openNow)
      mk i =
        let start = addUTCTime (fromIntegral ((i - 1) * 3600)) now
            end   = addUTCTime 3600 start
            newN  = newInWindow start end s
            openN = openNow + sum
                      [ newInWindow (addUTCTime (fromIntegral ((j - 1) * 3600)) now)
                                    (addUTCTime (fromIntegral (j * 3600)) now)
                                    s
                      | j <- [1..i]
                      ]
        in (end, newN, openN)

  in map mk ([0..23] :: [Int])

--------------------------------------------------------------------------------
-- Assignments (to get what's available now)
--------------------------------------------------------------------------------

data SrsStage = Initiate | Apprentice | Guru | Master | Enlightened | Burned
  deriving (Show, Eq)

srsStageLabel :: SrsStage -> String
srsStageLabel Initiate   = "Initiate"
srsStageLabel Apprentice = "Apprentice"
srsStageLabel Guru       = "Guru"
srsStageLabel Master     = "Master"
srsStageLabel Enlightened = "Enlightened"
srsStageLabel Burned     = "Burned"

srsStageFromInt :: Int -> SrsStage
srsStageFromInt 0         = Initiate
srsStageFromInt n | n <= 4 = Apprentice
srsStageFromInt n | n <= 6 = Guru
srsStageFromInt 7         = Master
srsStageFromInt 8         = Enlightened
srsStageFromInt _         = Burned

data Assignment = Assignment
  { asId        :: AssignmentId
  , asSubjectId :: SubjectId
  , asSrsStage  :: SrsStage
  } deriving (Show, Eq)

data AssignmentData = AssignmentData
  { adId       :: AssignmentId
  , adSubject  :: SubjectId
  , adSrsStage :: Int
  } deriving (Show)

-- | A WaniKani "collection" response: the requested page of @data@, plus
-- @pages.next_url@ if more pages remain. WaniKani paginates at up to 500
-- items per page -- without following this, a request matching more than
-- one page's worth of results (e.g. >500 overdue reviews) would silently
-- return only the first page with no indication anything was cut off.
data PagedEnvelope a = PagedEnvelope
  { peData    :: [a]
  , peNextUrl :: Maybe Text
  } deriving (Show)

instance FromJSON a => FromJSON (PagedEnvelope a) where
  parseJSON = withObject "PagedEnvelope" $ \o -> do
    d     <- o .: "data"
    pages <- o .:? "pages"
    next  <- maybe (pure Nothing) (.: "next_url") pages
    pure (PagedEnvelope d next)

instance FromJSON AssignmentData where
  parseJSON = withObject "AssignmentData" $ \o -> do
    i     <- o .: "id"
    d     <- o .: "data"
    s     <- d .: "subject_id"
    stage <- d .: "srs_stage"
    pure (AssignmentData i s stage)

toAssignment :: AssignmentData -> Assignment
toAssignment (AssignmentData i s stage) =
  Assignment i s (srsStageFromInt stage)

getAvailableAssignments :: String -> UTCTime -> Int -> IO [Assignment]
getAvailableAssignments token now n = do
  let nowParam = T.pack (iso8601Show now)

  firstPage <- runReq retryingHttpConfig $ do
    resp <- req
      GET
      (https "api.wanikani.com" /: "v2" /: "assignments")
      NoReqBody
      jsonResponse
      ( "available_before" =: nowParam
     <> "in_review"        =: True
     <> "hidden"           =: False
     <> apiOpts token )
    pure (responseBody resp :: PagedEnvelope AssignmentData)

  as <- collectPages token n firstPage
  pure (take n (map toAssignment as))

-- | Keep following @pages.next_url@ (see 'PagedEnvelope') until either the
-- requested count @n@ is reached or WaniKani reports no further pages.
collectPages :: FromJSON a => String -> Int -> PagedEnvelope a -> IO [a]
collectPages token n page
  | length (peData page) >= n = pure (peData page)
  | otherwise = case peNextUrl page of
      Nothing  -> pure (peData page)
      Just url -> do
        nextPage <- fetchPage token url
        rest <- collectPages token (n - length (peData page)) nextPage
        pure (peData page ++ rest)

-- | Fetch one collection page from an absolute URL (as found in
-- @pages.next_url@), reusing the same auth/revision headers as the initial
-- typed request.
fetchPage :: FromJSON a => String -> Text -> IO (PagedEnvelope a)
fetchPage token url = do
  uri <- mkURI url
  case useURI uri of
    Just (Right (u, opts)) -> runReq retryingHttpConfig $ do
      resp <- req GET u NoReqBody jsonResponse (opts <> apiOpts token)
      pure (responseBody resp)
    _ -> fail ("kroki: unexpected next_url, not an https URL: " <> T.unpack url)

-- | Fetch assignments for specific subjects (regardless of review
-- availability) -- used to show the current SRS stage for a fixed set of
-- subjects, e.g. leech-only practice sessions. Chunked like
-- 'getSubjectsByIds' to avoid huge URLs.
getAssignmentsBySubjectIds :: String -> [SubjectId] -> IO [Assignment]
getAssignmentsBySubjectIds token ids =
  map toAssignment
    <$> fetchBySubjectIdsChunked token (https "api.wanikani.com" /: "v2" /: "assignments") "subject_ids" ids

-- | Which of the given assignment ids are still due for review right now
-- (same "available" filter as 'getAvailableAssignments'). Used to detect a
-- create-review POST that actually reached WaniKani despite the client
-- seeing a connection failure on the /response/ -- resubmitting that one
-- would just be rejected by WaniKani forever, since an assignment stops
-- being "available" the moment a review is recorded for it.
getStillAvailableAssignmentIds :: String -> UTCTime -> [AssignmentId] -> IO (Set AssignmentId)
getStillAvailableAssignmentIds _ _ [] = pure Set.empty
getStillAvailableAssignmentIds token now ids = do
  let nowParam = T.pack (iso8601Show now)
      fetchAssignmentIdsChunk idsChunk = runReq retryingHttpConfig $ do
        let idsParam = T.intercalate "," (map (T.pack . show . unAssignmentId) idsChunk)
        resp <- req
          GET
          (https "api.wanikani.com" /: "v2" /: "assignments")
          NoReqBody
          jsonResponse
          ( "ids"              =: idsParam
         <> "available_before" =: nowParam
         <> "in_review"        =: True
         <> "hidden"           =: False
         <> apiOpts token )
        pure (map toAssignment (peData (responseBody resp :: PagedEnvelope AssignmentData)))
  results <- mapM fetchAssignmentIdsChunk (chunkN 100 ids)
  pure (Set.fromList (map asId (concat results)))

--------------------------------------------------------------------------------
-- Subjects (to show prompts + accepted answers)
--------------------------------------------------------------------------------

data SubjectType = Radical | Kanji | Vocabulary | KanaVocabulary
  deriving (Show, Eq)

data Subject = Subject
  { subjId               :: SubjectId
  , subjType             :: SubjectType
  , subjLevel            :: Int
  , subjChars            :: Maybe Text
  , subjMeanings         :: [Text]       -- accepted meanings
  , subjReadings         :: [Text]       -- accepted readings (kana/romaji depending on type)
  , subjAudioUrls        :: [Text]       -- pronunciation audio URLs (vocab only)
  , subjMeaningMnemonic  :: Maybe Text
  , subjReadingMnemonic  :: Maybe Text
  , subjComponentIds     :: [SubjectId]  -- radicals for kanji; kanji for vocab
  , subjAmalgamationIds  :: [SubjectId]  -- vocab for kanji; kanji for radical
  , subjVisuallySimilarIds :: [SubjectId] -- visually similar kanji (kanji only)
  } deriving (Show, Eq)

newtype PronAudio = PronAudio { paUrl :: Text }

instance FromJSON PronAudio where
  parseJSON = withObject "PronAudio" $ \o -> PronAudio <$> o .: "url"

instance FromJSON Subject where
  parseJSON = withObject "Subject" $ \o -> do
    sid <- o .: "id"
    obj <- o .: "object"
    st  <- parseSubjectType obj
    d   <- o .: "data"

    lvl   <- d .:  "level"
    chars <- d .:? "characters"

    meanings <- d .: "meanings" >>= parseAccepted "meaning"
    readings <- case st of
      Radical -> pure []
      _       -> d .:? "readings" >>= maybe (pure []) (parseAccepted "reading")

    let fetchAudio = maybe [] (map paUrl) <$> (d .:? "pronunciation_audios")
    audioUrls <- case st of
      Vocabulary     -> fetchAudio
      KanaVocabulary -> fetchAudio
      _              -> pure []

    mmnem   <- d .:? "meaning_mnemonic"
    rmnem   <- case st of
      Radical -> pure Nothing
      _       -> d .:? "reading_mnemonic"
    compIds <- case st of
      Radical -> pure []
      _       -> fromMaybe [] <$> (d .:? "component_subject_ids")
    amalgIds <- fromMaybe [] <$> (d .:? "amalgamation_subject_ids")
    simIds   <- case st of
      Kanji -> fromMaybe [] <$> (d .:? "visually_similar_subject_ids")
      _     -> pure []

    pure Subject
      { subjId              = sid
      , subjType            = st
      , subjLevel           = lvl
      , subjChars           = chars
      , subjMeanings        = meanings
      , subjReadings        = readings
      , subjAudioUrls       = audioUrls
      , subjMeaningMnemonic = mmnem
      , subjReadingMnemonic = rmnem
      , subjComponentIds    = compIds
      , subjAmalgamationIds = amalgIds
      , subjVisuallySimilarIds = simIds
      }

parseSubjectType :: Text -> Parser SubjectType
parseSubjectType t =
  case t of
    "radical"         -> pure Radical
    "kanji"           -> pure Kanji
    "vocabulary"      -> pure Vocabulary
    "kana_vocabulary" -> pure KanaVocabulary
    _                 -> fail ("Unknown subject type: " <> T.unpack t)

-- Parse accepted answers from a list of objects like:
-- { "meaning": "...", "accepted_answer": true, ... }
parseAccepted :: Text -> [AesonObj] -> Parser [Text]
parseAccepted field xs =
  fmap catMaybes $ mapM (acceptedFrom field) xs

type AesonObj = Object

acceptedFrom :: Text -> AesonObj -> Parser (Maybe Text)
acceptedFrom field o = do
  acc <- o .: "accepted_answer"
  if acc
    then Just <$> o .: Key.fromText field
    else pure Nothing


-- Fetch subjects by IDs; chunk to avoid huge URLs.
getSubjectsByIds :: String -> [SubjectId] -> IO [Subject]
getSubjectsByIds token = fetchBySubjectIdsChunked token (https "api.wanikani.com" /: "v2" /: "subjects") "ids"

-- | Fetch a resource keyed by subject id, chunked into groups of 100 to
-- avoid overlong URLs. Shared by 'getSubjectsByIds' and
-- 'getAssignmentsBySubjectIds', which differ only in the endpoint and the
-- query parameter name carrying the id list. Each chunk requests exactly
-- 100 specific ids, so (unlike 'getAvailableAssignments') the response
-- always fits in a single page.
fetchBySubjectIdsChunked :: FromJSON a => String -> Url 'Https -> Text -> [SubjectId] -> IO [a]
fetchBySubjectIdsChunked token endpoint paramName ids =
  fmap concat $ mapM (fetchChunk token endpoint paramName) (chunkN 100 ids)

fetchChunk :: FromJSON a => String -> Url 'Https -> Text -> [SubjectId] -> IO [a]
fetchChunk token endpoint paramName idsChunk = runReq retryingHttpConfig $ do
  let idsParam = T.intercalate "," (map (T.pack . show . unSubjectId) idsChunk)

  resp <- req
    GET
    endpoint
    NoReqBody
    jsonResponse
    ( paramName =: idsParam <> apiOpts token )

  pure (peData (responseBody resp))

chunkN :: Int -> [a] -> [[a]]
chunkN n0 = go
  where
    n = max 1 n0
    go [] = []
    go xs =
      let (a, b) = splitAt n xs
      in a : go b

-- | Outcome of a review POST as reported by WaniKani. We trust their
-- ending_srs_stage rather than computing it locally so the displayed
-- result can never drift from what is actually persisted.
data ReviewResult = ReviewResult
  { rrEndingSrsStage    :: SrsStage
  , rrEndingSrsStageNum :: Int
  } deriving (Show, Eq)

newtype ReviewEnvelope = ReviewEnvelope { reEnding :: Int } deriving (Show)

instance FromJSON ReviewEnvelope where
  parseJSON = withObject "ReviewEnvelope" $ \o -> do
    d <- o .: "data"
    ReviewEnvelope <$> d .: "ending_srs_stage"

createReview :: String -> AssignmentId -> Int -> Int -> UTCTime -> IO ReviewResult
createReview token assignmentId wrongMeaning wrongReading createdAt =
  runReq retryingHttpConfig $ do
    let body =
          object
            [ "review" .= object
                [ "assignment_id"             .= unAssignmentId assignmentId
                , "incorrect_meaning_answers" .= wrongMeaning
                , "incorrect_reading_answers" .= wrongReading
                , "created_at"                .= iso8601Show createdAt
                ]
            ]

    resp <- req
      POST
      (https "api.wanikani.com" /: "v2" /: "reviews")
      (ReqBodyJson body)
      jsonResponse
      (apiOpts token)

    let endingNum = reEnding (responseBody resp :: ReviewEnvelope)
    pure ReviewResult
      { rrEndingSrsStage    = srsStageFromInt endingNum
      , rrEndingSrsStageNum = endingNum
      }
