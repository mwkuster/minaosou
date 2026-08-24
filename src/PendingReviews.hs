{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Review submissions that a study session already answered locally but
-- couldn't get to WaniKani's servers -- every retry inside 'Api.createReview'
-- was exhausted (a longer network outage, the process being killed
-- mid-submit, etc.). The answer isn't lost, just unrecorded, so it's
-- persisted here and retried at the start of the next run instead of being
-- silently dropped.
module PendingReviews
  ( PendingReview(..)
  , pendingReviewsPath
  , loadPendingReviews
  , savePendingReviews
  , addPending
  , retryCreatedAt
  , clockSkewMargin
  ) where

import qualified Api
import JsonStore (configFilePath, loadJsonFile, saveJsonFile)

import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.:), (.=))
import Data.List (unionBy)
import Data.Function (on)
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime)

data PendingReview = PendingReview
  { prAssignmentId :: Api.AssignmentId
  , prWrongMeaning :: Int
  , prWrongReading :: Int
  , prCreatedAt    :: UTCTime
  } deriving (Show, Eq)

instance ToJSON PendingReview where
  toJSON p = object
    [ "assignment_id" .= prAssignmentId p
    , "wrong_meaning" .= prWrongMeaning p
    , "wrong_reading" .= prWrongReading p
    , "created_at"    .= prCreatedAt p
    ]

instance FromJSON PendingReview where
  parseJSON = withObject "PendingReview" $ \o ->
    PendingReview
      <$> o .: "assignment_id"
      <*> o .: "wrong_meaning"
      <*> o .: "wrong_reading"
      <*> o .: "created_at"

pendingReviewsPath :: IO FilePath
pendingReviewsPath = configFilePath "pending_reviews.json"

-- | Soft-fails to an empty list on a missing or corrupt file, matching
-- Config.loadConfig's and History.loadHistory's convention.
loadPendingReviews :: IO [PendingReview]
loadPendingReviews = loadJsonFile [] =<< pendingReviewsPath

-- | Best-effort, atomic write; a failure here must not crash the session.
savePendingReviews :: [PendingReview] -> IO ()
savePendingReviews prs = do
  path <- pendingReviewsPath
  saveJsonFile path prs

-- | Merge freshly-failed submissions into an existing pending list, keyed by
-- assignment id -- an assignment can only be due once at a time, so if it's
-- already pending from an earlier run, keep that original entry (and its
-- original 'created_at', the actual time the review happened) rather than
-- overwriting it with a new one.
addPending :: [PendingReview] -> [PendingReview] -> [PendingReview]
addPending existing new = unionBy ((==) `on` prAssignmentId) existing new

-- | How far in the past a stored 'created_at' has to be before it is safe to
-- send. WaniKani rejects a review timestamped ahead of its own clock, so a
-- local clock running fast can poison the value we saved (see 'Api.
-- reviewBody'). A minute is far more than any plausible drift between two
-- NTP-disciplined machines, and far less than the gap between the session
-- that failed and the next run that retries it.
clockSkewMargin :: NominalDiffTime
clockSkewMargin = 60

-- | The 'created_at' to resubmit a pending review with, or 'Nothing' to let
-- WaniKani stamp it from its own clock.
--
-- The stored time is what we want -- the review happened then, and WaniKani
-- schedules the next one from it -- but only while it is safely in the past.
-- A timestamp that is barely older than now, or ahead of it, is more likely
-- to be a fast clock than a real answer time, and sending it would fail the
-- retry exactly the way the original submission failed. Deferring to the
-- server's clock costs at most the delay between the two runs.
retryCreatedAt :: UTCTime -> PendingReview -> Maybe UTCTime
retryCreatedAt now p
  | diffUTCTime now (prCreatedAt p) > clockSkewMargin = Just (prCreatedAt p)
  | otherwise                                         = Nothing
