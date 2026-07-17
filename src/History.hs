{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Cross-session "leech" tracking: how often a subject has been answered
-- wrong across past study sessions, persisted locally since WaniKani's own
-- SRS stage can't distinguish "just leveled up" from "keeps regressing."
module History
  ( LeechEntry(..)
  , historyPath
  , loadHistory
  , saveHistory
  , mergeSession
  , historyCounts
  ) where

import qualified Api

import Control.Exception (IOException, catch)
import Data.Aeson (FromJSON(..), ToJSON(..), decode, encode, object, withObject, (.:), (.=))
import qualified Data.ByteString.Lazy as BL
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Time (UTCTime)
import System.Directory (createDirectoryIfMissing, getXdgDirectory, XdgDirectory(XdgConfig))
import System.FilePath ((</>))

data LeechEntry = LeechEntry
  { leSubjectId    :: Api.SubjectId
  , leWrongMeaning :: Int
  , leWrongReading :: Int
  , leLastSeen     :: UTCTime
  } deriving (Show, Eq)

instance ToJSON LeechEntry where
  toJSON e = object
    [ "subject_id"    .= leSubjectId e
    , "wrong_meaning" .= leWrongMeaning e
    , "wrong_reading" .= leWrongReading e
    , "last_seen"     .= leLastSeen e
    ]

instance FromJSON LeechEntry where
  parseJSON = withObject "LeechEntry" $ \o ->
    LeechEntry
      <$> o .: "subject_id"
      <*> o .: "wrong_meaning"
      <*> o .: "wrong_reading"
      <*> o .: "last_seen"

historyPath :: IO FilePath
historyPath = do
  base <- getXdgDirectory XdgConfig "kroki"
  pure (base </> "leeches.json")

-- | Soft-fails to an empty history on a missing or corrupt file, matching
-- Config.loadConfig's convention (never crash a study session over this).
loadHistory :: IO (M.Map Api.SubjectId LeechEntry)
loadHistory = do
  path <- historyPath
  content <- BL.readFile path `catch` \(_ :: IOException) -> pure BL.empty
  pure $ case decode content of
    Just entries -> M.fromList [ (leSubjectId e, e) | e <- entries ]
    Nothing      -> M.empty

-- | Best-effort write; a failure here must not crash session-end.
saveHistory :: M.Map Api.SubjectId LeechEntry -> IO ()
saveHistory history = do
  path <- historyPath
  base <- getXdgDirectory XdgConfig "kroki"
  ( do
      createDirectoryIfMissing True base
      BL.writeFile path (encode (M.elems history))
    ) `catch` \(_ :: IOException) -> pure ()

-- | Add this session's wrong counts on top of existing entries, bumping
-- last_seen to now. Only pass subjects with at least one wrong answer this
-- session, so clean subjects never get a spurious zero-count entry.
mergeSession
  :: UTCTime
  -> [(Api.SubjectId, Int, Int)]
  -> M.Map Api.SubjectId LeechEntry
  -> M.Map Api.SubjectId LeechEntry
mergeSession now sessionCounts existing = foldl' step existing sessionCounts
  where
    step acc (sid, wm, wr) =
      M.insertWith combine sid (LeechEntry sid wm wr now) acc
      where
        combine new old = old
          { leWrongMeaning = leWrongMeaning old + leWrongMeaning new
          , leWrongReading = leWrongReading old + leWrongReading new
          , leLastSeen     = leLastSeen new
          }

historyCounts :: M.Map Api.SubjectId LeechEntry -> M.Map Api.SubjectId (Int, Int)
historyCounts = M.map (\e -> (leWrongMeaning e, leWrongReading e))
