{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tui.Event
  ( handleEvent
  , shuffle
  , autoplayIfNeeded
  ) where

import qualified Api
import Tui.State

import Brick
import Brick.BChan (writeBChan)
import qualified Graphics.Vty as V

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Time (UTCTime)
import qualified Data.Text as T
import System.Process (spawnProcess)
import System.Random (randomRIO)

handleEvent :: IO (UTCTime, Api.Summary) -> ([Submission] -> IO SubmitResult) -> BrickEvent Name AppEvent -> EventM Name AppState ()
handleEvent _ _ (AppEvent (SubmitDone result)) =
  modify $ \st -> case result of
    Right r -> st
      { stMode          = Finished
      , stBanner        = Just (T.pack (srMessage r))
      , stError         = Nothing
      , stHasMore       = srHasMore r
      , stSubmitDetails = srDetails r
      }
    Left e -> st
      { stMode  = Finished
      , stBanner = Nothing
      , stError = Just (T.pack ("submit failed: " <> shortErr e))
      }
handleEvent refreshFn submitFn (VtyEvent ev) = do
  st <- get
  if stOverlay st /= NoOverlay
    then handleOverlay ev
    else do
      case stMode st of
        WrongAnswer _ _ -> handleWrongAnswer refreshFn ev
        ConfirmSubmit   -> handleConfirm submitFn ev
        Submitting      -> pure ()                           -- swallow all input
        Finished        -> handleFinished refreshFn ev
        _               -> handleNormal refreshFn ev
      autoplayIfNeeded
handleEvent _ _ _ = pure ()

-- | Auto-play the current question's audio on its first appearance this
-- session (config-gated). Called after every key event regardless of which
-- handler ran, so it can't be missed when a new handler advances the queue.
autoplayIfNeeded :: EventM Name AppState ()
autoplayIfNeeded = do
  st <- get
  case currentQuestion st of
    Just q | shouldAutoplay st q -> do
      put (markAutoplayed q st)
      liftIO $ playAudio (stAudioPlayer st) (qSubject q)
    _ -> pure ()

-- | Refresh summary and open the review-schedule overlay. On network error,
-- leave the overlay closed and surface the error in stError instead of
-- letting the exception bubble out and crash the TUI.
openReviewSchedule :: IO (UTCTime, Api.Summary) -> EventM Name AppState ()
openReviewSchedule refreshFn = do
  result <- liftIO (try refreshFn)
  case result of
    Right (now', summary') ->
      modify $ \st -> st
        { stOverlay = ReviewSchedule
        , stNow     = now'
        , stSummary = summary'
        , stError   = Nothing
        }
    Left (e :: SomeException) ->
      modify $ \st -> st
        { stError = Just (T.pack ("review schedule unavailable: " <> shortErr e)) }

-- | Truncate exception text so a long backtrace doesn't blow up the layout.
shortErr :: SomeException -> String
shortErr e =
  let msg = displayException e
      oneLine = takeWhile (/= '\n') msg
  in if length oneLine > 200 then take 197 oneLine <> "..." else oneLine

handleOverlay :: V.Event -> EventM Name AppState ()
handleOverlay ev =
  case ev of
    V.EvKey (V.KChar 'a') [V.MCtrl] -> close
    V.EvKey (V.KChar 'u') [V.MCtrl] -> close
    V.EvKey (V.KChar 'v') [V.MCtrl] -> close
    V.EvKey V.KEsc []                -> close
    V.EvKey V.KEnter []              -> closeIfAllInfo
    V.EvKey V.KUp []                 -> scroll (-1)
    V.EvKey V.KDown []               -> scroll 1
    V.EvKey (V.KChar 'k') []         -> scroll (-1)
    V.EvKey (V.KChar 'j') []         -> scroll 1
    _                                -> pure ()
  where
    close = modify $ \st -> st { stOverlay = NoOverlay }
    closeIfAllInfo = do
      st <- get
      case stOverlay st of
        AllInfo -> close
        _       -> pure ()
    scroll n = do
      st <- get
      let vp = case stOverlay st of
                 AllInfo        -> viewportScroll InfoViewport
                 UserInfo       -> viewportScroll UserViewport
                 ReviewSchedule -> viewportScroll ReviewViewport
                 NoOverlay      -> error "scroll called with NoOverlay"
      vScrollBy vp n

handleWrongAnswer :: IO (UTCTime, Api.Summary) -> V.Event -> EventM Name AppState ()
handleWrongAnswer refreshFn ev =
  case ev of
    V.EvKey (V.KChar 'o') [V.MCtrl] -> do
      st <- get
      case currentQuestion st of
        Nothing -> pure ()
        Just q  -> put (advanceOverride q st { stMode = Normal })

    V.EvKey (V.KChar 'r') [V.MCtrl] -> do
      st <- get
      case currentQuestion st of
        Nothing -> pure ()
        Just q  -> put (requeueOnly q st { stMode = Normal })

    V.EvKey (V.KChar 'p') [V.MCtrl] -> do
      st <- get
      case currentQuestion st of
        Just q | hasAudio q st -> liftIO $ playAudio (stAudioPlayer st) (qSubject q)
        _ -> pure ()

    V.EvKey (V.KChar 'a') [V.MCtrl] ->
      modify $ \st -> st { stOverlay = AllInfo }
    V.EvKey (V.KChar 'u') [V.MCtrl] ->
      modify $ \st -> st { stOverlay = UserInfo }
    V.EvKey (V.KChar 'v') [V.MCtrl] -> openReviewSchedule refreshFn

    V.EvKey V.KEnter [] -> do
      st <- get
      case currentQuestion st of
        Nothing -> pure ()
        Just q  -> put (requeueWrong q st { stMode = Normal })

    V.EvKey V.KEsc [] -> do
      st <- get
      put st { stMode = Normal }

    _ -> pure ()

handleConfirm :: ([Submission] -> IO SubmitResult) -> V.Event -> EventM Name AppState ()
handleConfirm submitFn ev =
  case ev of
    V.EvKey (V.KChar 'y') [] -> doSubmit
    V.EvKey V.KEnter []      -> doSubmit
    V.EvKey (V.KChar 'n') [] -> do
      st <- get
      put st { stMode = Finished }

    V.EvKey V.KEsc [] -> do
      st <- get
      put st { stMode = Finished }

    _ -> pure ()
  where
    doSubmit = do
      st <- get
      let chan = stSubmitChan st
          subs = mkSubmissions st
      put st
        { stMode   = Submitting
        , stBanner = Just "Submitting to WaniKani…"
        , stError  = Nothing
        }
      void $ liftIO $ forkIO $ do
        r <- try (submitFn subs)
        writeBChan chan (SubmitDone r)

handleFinished :: IO (UTCTime, Api.Summary) -> V.Event -> EventM Name AppState ()
handleFinished refreshFn ev =
  case ev of
    V.EvKey (V.KChar 'q') [V.MCtrl] -> halt
    V.EvKey V.KEsc []               -> halt
    V.EvKey (V.KChar 'u') [V.MCtrl] ->
      modify $ \st -> st { stOverlay = UserInfo }
    V.EvKey (V.KChar 'v') [V.MCtrl] -> openReviewSchedule refreshFn
    V.EvKey (V.KChar 's') [V.MCtrl] -> do
      st <- get
      case stBanner st of
        Nothing -> put st { stMode = ConfirmSubmit }
        Just _  -> pure ()
    V.EvKey (V.KChar 'n') [V.MCtrl] -> do
      st <- get
      if stHasMore st
        then put st { stWantsMore = True } >> halt
        else pure ()
    V.EvKey V.KUp []         -> vScrollBy (viewportScroll DoneViewport) (-1)
    V.EvKey V.KDown []       -> vScrollBy (viewportScroll DoneViewport) 1
    V.EvKey (V.KChar 'k') [] -> vScrollBy (viewportScroll DoneViewport) (-1)
    V.EvKey (V.KChar 'j') [] -> vScrollBy (viewportScroll DoneViewport) 1
    _                               -> pure ()

handleNormal :: IO (UTCTime, Api.Summary) -> V.Event -> EventM Name AppState ()
handleNormal refreshFn ev =
  case ev of
    V.EvKey (V.KChar 'q') [V.MCtrl] ->
      halt

    V.EvKey (V.KChar 'o') [V.MCtrl] -> do
      st <- get
      case currentQuestion st of
        Nothing -> pure ()
        Just q  -> put (advanceOverride q st)

    V.EvKey (V.KChar 'r') [V.MCtrl] -> do
      st <- get
      case currentQuestion st of
        Nothing -> pure ()
        Just q  -> put (requeueOnly q st)

    V.EvKey (V.KChar 'p') [V.MCtrl] -> do
      st <- get
      case currentQuestion st of
        Just q | hasAudio q st -> liftIO $ playAudio (stAudioPlayer st) (qSubject q)
        _ -> pure ()

    V.EvKey (V.KChar 'a') [V.MCtrl] ->
      modify $ \st -> st { stOverlay = AllInfo }
    V.EvKey (V.KChar 'u') [V.MCtrl] ->
      modify $ \st -> st { stOverlay = UserInfo }
    V.EvKey (V.KChar 'v') [V.MCtrl] -> openReviewSchedule refreshFn

    V.EvKey V.KEsc [] ->
      halt

    V.EvKey V.KEnter [] -> do
      st <- get
      case currentQuestion st of
        Nothing -> pure ()
        Just q  ->
          let ans = T.strip (stInput st)
          in if T.null ans then pure () else put (submitAnswer q ans st)

    V.EvKey V.KBS [] -> do
      st <- get
      put st
        { stInput = if T.null (stInput st) then T.empty else T.init (stInput st)
        , stMode  = Normal
        }

    V.EvKey V.KDel [] -> do
      st <- get
      put st
        { stInput = if T.null (stInput st) then T.empty else T.init (stInput st)
        , stMode  = Normal
        }

    V.EvKey (V.KChar c) [] -> do
      st <- get
      put st
        { stInput = stInput st <> T.singleton c
        , stMode  = Normal
        , stError = Nothing
        }

    _ -> pure ()

-- | Fire-and-forget audio playback via configured external player.
playAudio :: Maybe String -> Api.Subject -> IO ()
playAudio Nothing _ = pure ()
playAudio (Just cmd) subj =
  case Api.subjAudioUrls subj of
    [] -> pure ()
    urls -> do
      i <- randomRIO (0, length urls - 1)
      let url         = urls !! i
          (exe, args) = case words cmd of
                          []     -> ("mpv", [])
                          (w:ws) -> (w, ws)
      void $ spawnProcess exe (args ++ [T.unpack url])

shuffle :: [a] -> IO [a]
shuffle xs = go xs []
  where
    go [] acc = pure acc
    go ys acc = do
      i <- randomRIO (0, length ys - 1)
      case splitAt i ys of
        (front, a:back) -> go (front ++ back) (a : acc)
        _               -> pure acc
