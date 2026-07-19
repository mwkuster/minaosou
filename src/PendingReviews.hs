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
  ) where

import qualified Api

import Control.Exception (IOException, catch)
import Data.Aeson (FromJSON(..), ToJSON(..), decode, encode, object, withObject, (.:), (.=))
import qualified Data.ByteString.Lazy as BL
import Data.List (unionBy)
import Data.Function (on)
import Data.Time (UTCTime)
import System.Directory (createDirectoryIfMissing, getXdgDirectory, XdgDirectory(XdgConfig))
import System.FilePath ((</>))

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
pendingReviewsPath = do
  base <- getXdgDirectory XdgConfig "kroki"
  pure (base </> "pending_reviews.json")

-- | Soft-fails to an empty list on a missing or corrupt file, matching
-- Config.loadConfig's and History.loadHistory's convention.
loadPendingReviews :: IO [PendingReview]
loadPendingReviews = do
  path <- pendingReviewsPath
  content <- BL.readFile path `catch` \(_ :: IOException) -> pure BL.empty
  pure (maybe [] id (decode content))

-- | Best-effort write; a failure here must not crash the session.
savePendingReviews :: [PendingReview] -> IO ()
savePendingReviews prs = do
  path <- pendingReviewsPath
  base <- getXdgDirectory XdgConfig "kroki"
  ( do
      createDirectoryIfMissing True base
      BL.writeFile path (encode prs)
    ) `catch` \(_ :: IOException) -> pure ()

-- | Merge freshly-failed submissions into an existing pending list, keyed by
-- assignment id -- an assignment can only be due once at a time, so if it's
-- already pending from an earlier run, keep that original entry (and its
-- original 'created_at', the actual time the review happened) rather than
-- overwriting it with a new one.
addPending :: [PendingReview] -> [PendingReview] -> [PendingReview]
addPending existing new = unionBy ((==) `on` prAssignmentId) existing new
