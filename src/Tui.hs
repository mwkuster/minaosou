{-# LANGUAGE OverloadedStrings #-}

-- | Facade for the Tui subsystem. Re-exports the public surface from
-- 'Tui.State' and provides the entry point 'runStudyTui'.
module Tui
  ( module Tui.State
  , runStudyTui
  ) where

import qualified Api
import Tui.State
import Tui.Draw (drawUi, theMap)
import Tui.Event (handleEvent, shuffle, autoplayIfNeeded)

import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (bracket)
import Control.Monad (forever)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.LocalTime (TimeZone)

runStudyTui
  :: StudyConfig
  -> Api.User -> Api.Summary -> UTCTime -> TimeZone
  -> M.Map Api.SubjectId Api.Subject
  -> M.Map Api.SubjectId Api.Assignment
  -> M.Map Api.SubjectId (Int, Int)
  -> [Api.Subject]
  -> IO (UTCTime, Api.Summary)
  -> ([Submission] -> IO SubmitResult)
  -> (Api.SubjectId -> Maybe Api.StudyMaterialId -> [T.Text] -> IO Api.StudyMaterial)
  -> IO (Bool, Maybe [(Api.SubjectId, Int, Int)])
runStudyTui cfg user summary now tz allSubjects subjToAsg priorWrong subjects refreshFn submitFn submitSynonymFn = do
  let queue0 = concatMap mkQuestions subjects
      prog0  = M.fromList [ (Api.subjId s, initProgress s) | s <- subjects ]

  queue <- spaceOutSameSubject <$> shuffle queue0
  chan  <- newBChan 10
  -- The timer runs off the wall clock at TUI start, not the 'now' the caller
  -- fetched alongside the summary (which may be several API calls old).
  start <- getCurrentTime

  let st0 = AppState
        { stQueue        = queue
        , stQueueWidget  = mkQueueWidget queue
        , stInput        = T.empty
        , stProgress     = prog0
        , stSubjToAsg    = subjToAsg
        , stRequeueAfter = scRequeueAfter cfg
        , stCorrect      = 0
        , stWrong        = 0
        , stOverridden   = 0
        , stMode          = Normal
        , stBanner        = Nothing
        , stError         = Nothing
        , stNotice        = Nothing
        , stHasMore       = False
        , stWantsMore     = False
        , stAudioPlayer   = scAudioPlayer cfg
        , stSubmitDetails = []
        , stOverlay       = NoOverlay
        , stAllSubjects   = allSubjects
        , stUser          = user
        , stSummary       = summary
        , stNow           = now
        , stTZ            = tz
        , stSessionStart  = start
        , stClock         = start
        , stSubmitChan    = chan
        , stLastCompleted = Nothing
        , stPriorWrong    = priorWrong
        , stAudioAutoplay = scAudioAutoplay cfg
        , stAutoplayed    = S.empty
        , stPracticeOnly  = scPracticeOnly cfg
        , stSubmitAttempted = False
        }

  let buildVty = VCP.mkVty V.defaultConfig
  initialVty <- buildVty
  finalState <-
    withTicker chan $
      customMain initialVty buildVty (Just chan) (app refreshFn submitFn submitSynonymFn) st0
  pure (stWantsMore finalState, recordableWrongCounts finalState)

-- | Run an action with a background thread feeding the session timer a 'Tick'
-- once a second. The thread is killed when the action finishes, so a
-- multi-batch session doesn't accumulate one ticker per batch.
withTicker :: BChan AppEvent -> IO a -> IO a
withTicker chan =
  bracket (forkIO tick) killThread . const
  where
    tick = forever $ do
      threadDelay 1000000
      writeBChan chan . Tick =<< getCurrentTime

app
  :: IO (UTCTime, Api.Summary)
  -> ([Submission] -> IO SubmitResult)
  -> (Api.SubjectId -> Maybe Api.StudyMaterialId -> [T.Text] -> IO Api.StudyMaterial)
  -> App AppState AppEvent Name
app refreshFn submitFn submitSynonymFn = App
  { appDraw         = drawUi
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent refreshFn submitFn submitSynonymFn
  , appStartEvent   = autoplayIfNeeded
  , appAttrMap      = const theMap
  }
