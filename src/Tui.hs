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
import Tui.Event (handleEvent, shuffle)

import Brick
import Brick.BChan (newBChan)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.LocalTime (TimeZone)

runStudyTui :: Int -> Maybe String -> Api.User -> Api.Summary -> UTCTime -> TimeZone -> M.Map Api.SubjectId Api.Subject -> M.Map Api.SubjectId Api.Assignment -> [Api.Subject] -> IO (UTCTime, Api.Summary) -> ([Submission] -> IO SubmitResult) -> IO Bool
runStudyTui rqAfter audioPlayer user summary now tz allSubjects subjToAsg subjects refreshFn submitFn = do
  let queue0 = concatMap mkQuestions subjects
      prog0  = M.fromList [ (Api.subjId s, initProgress s) | s <- subjects ]

  queue <- spaceOutSameSubject <$> shuffle queue0
  chan  <- newBChan 10

  let st0 = AppState
        { stQueue        = queue
        , stQueueWidget  = mkQueueWidget queue
        , stInput        = T.empty
        , stProgress     = prog0
        , stSubjToAsg    = subjToAsg
        , stRequeueAfter = rqAfter
        , stCorrect      = 0
        , stWrong        = 0
        , stOverridden   = 0
        , stMode          = Normal
        , stBanner        = Nothing
        , stError         = Nothing
        , stHasMore       = False
        , stWantsMore     = False
        , stAudioPlayer   = audioPlayer
        , stSubmitDetails = []
        , stOverlay       = NoOverlay
        , stAllSubjects   = allSubjects
        , stUser          = user
        , stSummary       = summary
        , stNow           = now
        , stTZ            = tz
        , stSubmitChan    = chan
        , stLastCompleted = Nothing
        }

  let buildVty = VCP.mkVty V.defaultConfig
  initialVty <- buildVty
  finalState <- customMain initialVty buildVty (Just chan) (app refreshFn submitFn) st0
  pure (stWantsMore finalState)

app :: IO (UTCTime, Api.Summary) -> ([Submission] -> IO SubmitResult) -> App AppState AppEvent Name
app refreshFn submitFn = App
  { appDraw         = drawUi
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent refreshFn submitFn
  , appStartEvent   = pure ()
  , appAttrMap      = const theMap
  }
