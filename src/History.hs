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
  , applyPracticeSession
  , historyCounts
  , leechWeight
  , activeLeeches
  ) where

import qualified Api
import JsonStore (configFilePath, loadJsonFile, saveJsonFile)

import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.:), (.:?), (.!=), (.=))
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Time (UTCTime)

data LeechEntry = LeechEntry
  { leSubjectId    :: Api.SubjectId
  , leWrongMeaning :: Int
  , leWrongReading :: Int
  , leLastSeen     :: UTCTime
  , leRetired      :: Bool
    -- ^ Graduated out of active leech status via a clean
    -- @kroki leeches --study@ round. Excluded from the leech list and from
    -- future practice queues, but the record is kept (not deleted) so a
    -- relapse can still be recognised.
  , leRelapses     :: Int
    -- ^ Times this subject was retired and then answered wrong again in a
    -- real WaniKani review. Feeds 'leechWeight' so a relapsed leech ranks
    -- above a fresh one with the same raw miss count.
  } deriving (Show, Eq)

instance ToJSON LeechEntry where
  toJSON e = object
    [ "subject_id"    .= leSubjectId e
    , "wrong_meaning" .= leWrongMeaning e
    , "wrong_reading" .= leWrongReading e
    , "last_seen"     .= leLastSeen e
    , "retired"       .= leRetired e
    , "relapses"      .= leRelapses e
    ]

instance FromJSON LeechEntry where
  parseJSON = withObject "LeechEntry" $ \o ->
    LeechEntry
      <$> o .: "subject_id"
      <*> o .: "wrong_meaning"
      <*> o .: "wrong_reading"
      <*> o .: "last_seen"
      <*> o .:? "retired"  .!= False
      <*> o .:? "relapses" .!= 0

historyPath :: IO FilePath
historyPath = configFilePath "leeches.json"

-- | Soft-fails to an empty history on a missing or corrupt file, matching
-- Config.loadConfig's convention (never crash a study session over this).
loadHistory :: IO (M.Map Api.SubjectId LeechEntry)
loadHistory = do
  path <- historyPath
  entries <- loadJsonFile [] path
  pure (M.fromList [ (leSubjectId e, e) | e <- entries ])

-- | Best-effort, atomic write; a failure here must not crash session-end.
saveHistory :: M.Map Api.SubjectId LeechEntry -> IO ()
saveHistory history = do
  path <- historyPath
  saveJsonFile path (M.elems history)

-- | Add this session's wrong counts on top of existing entries, bumping
-- last_seen to now. Only pass subjects with at least one wrong answer this
-- session, so clean subjects never get a spurious zero-count entry.
--
-- A subject that was previously retired (graduated out of leech status via
-- a clean practice round) but shows up here -- i.e. missed again in a real
-- review -- is a relapse: it's un-retired immediately and 'leRelapses' is
-- bumped, so 'leechWeight' ranks it above a first-time leech with the same
-- raw miss count right away, rather than waiting for the miss count to
-- climb back up on its own.
mergeSession
  :: UTCTime
  -> [(Api.SubjectId, Int, Int)]
  -> M.Map Api.SubjectId LeechEntry
  -> M.Map Api.SubjectId LeechEntry
mergeSession now sessionCounts existing = foldl' step existing sessionCounts
  where
    step acc (sid, wm, wr) = case M.lookup sid acc of
      Just old | leRetired old ->
        M.insert sid old
          { leWrongMeaning = wm
          , leWrongReading = wr
          , leLastSeen     = now
          , leRetired      = False
          , leRelapses     = leRelapses old + 1
          } acc
      _ -> M.insertWith combine sid (LeechEntry sid wm wr now False 0) acc
      where
        combine new old = old
          { leWrongMeaning = leWrongMeaning old + leWrongMeaning new
          , leWrongReading = leWrongReading old + leWrongReading new
          , leLastSeen     = leLastSeen new
          }

-- | After a leech-only practice session ("kroki leeches --study"), update
-- each practiced subject's entry:
--
--   * answered fully correctly this round -> it "graduated": retire it
--     (excluded from the leech list and future practice queues) rather than
--     deleting the record, so a later relapse in a real review (see
--     'mergeSession') can still be recognised and weighted;
--   * still missed this round -> /add/ this round's mistakes to its counts
--     (per type: a meaning miss raises the meaning counter, a reading miss
--     the reading counter) and keep it active. A leech you keep failing even
--     in dedicated practice is a worse leech, so it should climb the
--     worst-first ordering, not merely hold its old score.
--
-- Only ever called for a completed practice session (see
-- 'Tui.recordableWrongCounts'), so every practiced subject was actually
-- answered and "absent from the miss list" reliably means "answered clean".
applyPracticeSession
  :: UTCTime
  -> [Api.SubjectId]
  -> [(Api.SubjectId, Int, Int)]
  -> M.Map Api.SubjectId LeechEntry
  -> M.Map Api.SubjectId LeechEntry
applyPracticeSession now practiced sessionCounts existing =
  foldl' step existing practiced
  where
    wrongMap = M.fromList [ (sid, (wm, wr)) | (sid, wm, wr) <- sessionCounts ]
    step acc sid = case M.lookup sid wrongMap of
      Nothing ->
        M.adjust (\e -> e { leRetired = True, leLastSeen = now }) sid acc
      Just (wm, wr) ->
        M.adjust
          (\e -> e
            { leWrongMeaning = leWrongMeaning e + wm
            , leWrongReading = leWrongReading e + wr
            , leLastSeen     = now
            , leRetired      = False
            })
          sid acc

historyCounts :: M.Map Api.SubjectId LeechEntry -> M.Map Api.SubjectId (Int, Int)
historyCounts = M.map (\e -> (leWrongMeaning e, leWrongReading e))

-- | Ranking weight for leech ordering (worst-first lists and practice
-- queues): raw miss counts, plus a bonus per relapse so a subject that
-- already graduated once and came back wrong outranks a fresh leech with
-- the same raw miss count.
leechWeight :: LeechEntry -> Int
leechWeight e = leWrongMeaning e + leWrongReading e + relapseBonus * leRelapses e
  where relapseBonus = 3

-- | Entries not yet retired -- i.e. still due to be surfaced in
-- @kroki leeches@ and practiced by @kroki leeches --study@.
activeLeeches :: M.Map Api.SubjectId LeechEntry -> M.Map Api.SubjectId LeechEntry
activeLeeches = M.filter (not . leRetired)
