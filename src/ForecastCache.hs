{-# LANGUAGE OverloadedStrings #-}

-- | Local cache of the two bulk collections @minaosou forecast@ reads.
--
-- A full sweep of an established account is a dozen-odd pages per endpoint,
-- and WaniKani allows 60 requests per minute per account. Three forecast
-- runs in a minute therefore used to fail outright with a 429 -- and the
-- in-process rate limiter cannot help, because each run is a fresh process
-- that cannot see what the previous one already spent.
--
-- Refetching a five-figure record set to answer a what-if question is the
-- real problem, though, not the budget it happens to breach. WaniKani
-- publishes a @data_updated_at@ watermark on every collection and accepts it
-- back as @updated_after@ precisely so a client need not do that. This
-- module keeps the last sweep on disk, so a repeat run asks only for what
-- has changed since -- typically a request or two.
module ForecastCache
  ( Cache(..)
  , emptyCache
  , cachePath
  , loadCache
  , saveCache
  , forToken
  , mergeById
  , tokenFingerprint
  ) where

import qualified Api
import JsonStore (configFilePath, loadJsonFile, saveJsonFile)

import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.:), (.:?), (.=))
import Data.Bits (xor)
import Data.Char (ord)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Time (UTCTime)
import Data.Word (Word64)

data Cache = Cache
  { cacheToken    :: Word64
    -- ^ Fingerprint of the token the cache was built with, so a different
    -- account never gets served another one's history. The token itself is
    -- deliberately not stored -- this file has no business holding a
    -- credential.
  , cacheStats    :: [Api.ReviewStat]
  , cacheStatsAt  :: Maybe UTCTime
  , cacheProgress :: [Api.Progress]
  , cacheProgAt   :: Maybe UTCTime
  } deriving (Show, Eq)

emptyCache :: Word64 -> Cache
emptyCache t = Cache t [] Nothing [] Nothing

instance ToJSON Cache where
  toJSON c = object
    [ "token_fingerprint" .= cacheToken c
    , "review_statistics" .= map statJson (cacheStats c)
    , "review_statistics_updated_at" .= cacheStatsAt c
    , "assignments" .= map progJson (cacheProgress c)
    , "assignments_updated_at" .= cacheProgAt c
    ]
    where
      statJson s = object
        [ "subject_id"    .= Api.unSubjectId (Api.rsSubjectId s)
        , "reviews"       .= Api.rsReviews s
        , "meaning_wrong" .= Api.rsMeaningWrong s
        , "reading_wrong" .= Api.rsReadingWrong s
        , "has_reading"   .= Api.rsHasReading s
        ]
      progJson p = object
        [ "subject_id" .= Api.unSubjectId (Api.pgSubjectId p)
        , "srs_stage"  .= Api.pgSrsStage p
        , "started_at" .= Api.pgStartedAt p
        , "burned_at"  .= Api.pgBurnedAt p
        ]

instance FromJSON Cache where
  parseJSON = withObject "Cache" $ \o -> do
    tok      <- o .: "token_fingerprint"
    statsRaw <- o .: "review_statistics"
    statsAt  <- o .:? "review_statistics_updated_at"
    progRaw  <- o .: "assignments"
    progAt   <- o .:? "assignments_updated_at"
    stats    <- mapM parseStat statsRaw
    prog     <- mapM parseProg progRaw
    pure (Cache tok stats statsAt prog progAt)
    where
      parseStat = withObject "ReviewStat" $ \o ->
        Api.ReviewStat
          <$> (Api.SubjectId <$> o .: "subject_id")
          <*> o .: "reviews"
          <*> o .: "meaning_wrong"
          <*> o .: "reading_wrong"
          <*> o .: "has_reading"
      parseProg = withObject "Progress" $ \o ->
        Api.Progress
          <$> (Api.SubjectId <$> o .: "subject_id")
          <*> o .: "srs_stage"
          <*> o .:? "started_at"
          <*> o .:? "burned_at"

cachePath :: IO FilePath
cachePath = configFilePath "forecast_cache.json"

-- | Soft-fails to an empty cache on a missing or corrupt file: a cache is an
-- optimisation, and a bad one must cost a full refetch, never a crash.
loadCache :: Word64 -> IO Cache
loadCache fingerprint = do
  path <- cachePath
  loadJsonFile (emptyCache fingerprint) path

saveCache :: Cache -> IO ()
saveCache c = do
  path <- cachePath
  saveJsonFile path c

-- | The cache as it applies to this token: the stored one if it was built
-- with the same token, an empty one otherwise. Switching accounts silently
-- refetches rather than reporting the previous account's history.
forToken :: String -> Cache -> Cache
forToken token c
  | cacheToken c == fingerprint = c
  | otherwise                   = emptyCache fingerprint
  where fingerprint = tokenFingerprint token

-- | Lay newly fetched records over cached ones, keyed by subject. An
-- incremental fetch returns whole records for whatever changed, so the fresh
-- copy wins outright and everything untouched survives.
mergeById :: Ord k => (a -> k) -> [a] -> [a] -> [a]
mergeById key cached fresh =
  M.elems (foldl' insert (M.fromList [ (key x, x) | x <- cached ]) fresh)
  where insert acc x = M.insert (key x) x acc

-- | FNV-1a over the token. Only ever compared against itself, so the bar is
-- "distinguishes two tokens", not cryptographic strength -- but it does have
-- to be one-way enough that the cache file is not a place a token leaks
-- from, which a plain copy would be.
tokenFingerprint :: String -> Word64
tokenFingerprint = foldl' step 14695981039346656037
  where
    step h ch = (h `xor` fromIntegral (ord ch)) * 1099511628211
