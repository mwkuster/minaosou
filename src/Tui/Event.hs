{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tui.Event
  ( handleEvent
  , shuffle
  , autoplayIfNeeded
  ) where

import qualified Api
import Tui.State
import Util (shortErr)

import Brick
import Brick.BChan (writeBChan)
import qualified Graphics.Vty as V

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Time (UTCTime)
import qualified Data.Text as T
import System.Process (spawnProcess)
import System.Random (randomRIO)

-- | How the event layer submits a meaning synonym: the subject, its existing
-- study-material id (if any), and the /complete/ new synonym list.
type SynonymSubmit =
  Api.SubjectId -> Maybe Api.StudyMaterialId -> [T.Text] -> IO Api.StudyMaterial

handleEvent
  :: IO (UTCTime, Api.Summary)
  -> ([Submission] -> IO SubmitResult)
  -> SynonymSubmit
  -> BrickEvent Name AppEvent
  -> EventM Name AppState ()
handleEvent _ _ _ (AppEvent (SubmitDone result)) =
  modify $ \st -> case result of
    Right r -> st
      { stMode          = Finished
      , stBanner        = Just (T.pack (srMessage r))
      , stError         = Nothing
      , stHasMore       = srHasMore r
      , stSubmitDetails = srDetails r
      , stSubmitAttempted = True
      }
    -- Even a wholesale failure counts as attempted: the answers are either
    -- recorded or queued in pending_reviews.json for the next run, so their
    -- misses belong in the leech history either way.
    Left e -> st
      { stMode  = Finished
      , stBanner = Nothing
      , stError = Just (T.pack ("submit failed: " <> shortErr e))
      , stSubmitAttempted = True
      }
handleEvent _ _ _ (AppEvent (SynonymDone result)) = handleSynonymDone result
handleEvent _ _ _ (AppEvent (Tick now)) =
  modify $ \st -> st { stClock = now }
handleEvent refreshFn submitFn submitSynonymFn (VtyEvent ev) = do
  st <- get
  if stOverlay st /= NoOverlay
    then handleOverlay ev
    else do
      case stMode st of
        WrongAnswer _ _     -> handleWrongAnswer refreshFn ev
        ConfirmSubmit       -> handleConfirm submitFn ev
        Submitting          -> pure ()                       -- swallow all input
        SynonymEntry _ _    -> handleSynonymEntry submitSynonymFn ev
        SynonymSubmitting _ -> pure ()                       -- swallow input in flight
        Finished            -> handleFinished refreshFn ev
        _                   -> handleNormal refreshFn ev
      autoplayIfNeeded
handleEvent _ _ _ _ = pure ()

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
    -- Total on purpose. This used to 'error' in the NoOverlay case, which
    -- was unreachable only by the caller's guard -- and an exception thrown
    -- here escapes 'customMain' entirely, skipping the end-of-session
    -- 'History.saveHistory' with no trace of why.
    scroll n = do
      st <- get
      case overlayViewport (stOverlay st) of
        Nothing -> pure ()
        Just vp -> vScrollBy (viewportScroll vp) n

-- | Ctrl-key actions shared between the Normal and WrongAnswer input modes:
-- override/requeue the current question, play audio, open an overlay or the
-- review schedule, and scroll the current-question viewport. 'Nothing' means
-- the event isn't one of these, so the caller falls through to its
-- mode-specific handling (answer submission vs. requeue-as-wrong, etc).
handleSharedCtrlKeys :: IO (UTCTime, Api.Summary) -> V.Event -> Maybe (EventM Name AppState ())
handleSharedCtrlKeys refreshFn ev = case ev of
  V.EvKey (V.KChar 'o') [V.MCtrl] -> Just $ do
    st <- get
    case currentQuestion st of
      Nothing -> pure ()
      Just q  -> put (advanceOverride q st) >> resetMainScroll

  V.EvKey (V.KChar 'r') [V.MCtrl] -> Just $ do
    st <- get
    case currentQuestion st of
      Nothing -> pure ()
      Just q  -> put (requeueOnly q st) >> resetMainScroll

  V.EvKey (V.KChar 'p') [V.MCtrl] -> Just $ do
    st <- get
    case currentQuestion st of
      Just q | hasAudio q st -> liftIO $ playAudio (stAudioPlayer st) (qSubject q)
      _ -> pure ()

  V.EvKey (V.KChar 'a') [V.MCtrl] -> Just $
    modify $ \st -> st { stOverlay = AllInfo }
  V.EvKey (V.KChar 'u') [V.MCtrl] -> Just $
    modify $ \st -> st { stOverlay = UserInfo }
  V.EvKey (V.KChar 'v') [V.MCtrl] -> Just $ openReviewSchedule refreshFn

  V.EvKey V.KUp []   -> Just $ vScrollBy (viewportScroll MainViewport) (-1)
  V.EvKey V.KDown [] -> Just $ vScrollBy (viewportScroll MainViewport) 1

  _ -> Nothing
  where
    resetMainScroll = vScrollToBeginning (viewportScroll MainViewport)

handleWrongAnswer :: IO (UTCTime, Api.Summary) -> V.Event -> EventM Name AppState ()
handleWrongAnswer refreshFn ev =
  case handleSharedCtrlKeys refreshFn ev of
    Just action -> action
    Nothing -> case ev of
      -- Add a meaning synonym to WaniKani (meaning questions only -- WaniKani
      -- has no reading synonyms). Opens an editable field pre-filled with the
      -- answer just given; the submit itself happens in handleSynonymEntry.
      V.EvKey (V.KChar 'y') [V.MCtrl] -> do
        st <- get
        case (stMode st, currentQuestion st) of
          (WrongAnswer inp acc, Just q)
            | qKind q == QMeaning ->
                put st { stMode  = SynonymEntry inp (inp, acc)
                       , stError = Nothing
                       , stNotice = Nothing
                       }
          _ -> pure ()

      V.EvKey V.KEnter [] -> do
        st <- get
        case currentQuestion st of
          Nothing -> pure ()
          Just q  -> put (requeueWrong q st) >> resetMainScroll

      V.EvKey V.KEsc [] -> do
        st <- get
        put st { stMode = Normal } >> resetMainScroll

      _ -> pure ()
  where
    resetMainScroll = vScrollToBeginning (viewportScroll MainViewport)

-- | Editing a meaning synonym (see the Ctrl-y case above). Enter validates
-- and, if valid, fires the async add and blocks input until it returns; Esc
-- cancels back to the wrong-answer screen. A validation failure (empty, too
-- long, duplicate, over the cap) is shown without any network call.
handleSynonymEntry :: SynonymSubmit -> V.Event -> EventM Name AppState ()
handleSynonymEntry submitSynonymFn ev = do
  st <- get
  case stMode st of
    SynonymEntry buf payload@(inp, acc) -> case ev of
      V.EvKey V.KEsc [] ->
        put st { stMode = WrongAnswer inp acc, stError = Nothing }

      V.EvKey V.KEnter [] ->
        case currentQuestion st of
          Nothing -> pure ()
          Just q  ->
            let subj     = qSubject q
                existing = Api.subjUserSynonyms subj
                accepted = acceptedMeanings subj
            in case mergeSynonym existing accepted buf of
                 Left reason   -> put st { stError = Just reason }
                 Right newList -> do
                   let chan  = stSubmitChan st
                       sid   = Api.subjId subj
                       mSmId = Api.subjStudyMaterialId subj
                   put st { stMode   = SynonymSubmitting payload
                          , stError  = Nothing
                          , stNotice = Nothing
                          }
                   void $ liftIO $ forkIO $ do
                     r <- try (submitSynonymFn sid mSmId newList)
                     writeBChan chan (SynonymDone r)

      V.EvKey k [] | k `elem` [V.KBS, V.KDel] ->
        put st { stMode  = SynonymEntry (if T.null buf then buf else T.init buf) payload
               , stError = Nothing
               }

      V.EvKey (V.KChar c) [] ->
        put st { stMode = SynonymEntry (buf <> T.singleton c) payload, stError = Nothing }

      _ -> pure ()
    _ -> pure ()

-- | Resolve an in-flight synonym add. On success, record the synonyms locally
-- so the rest of the session accepts them, then count the current question
-- correct (override) and advance. On failure, return to the wrong-answer
-- screen with the error.
handleSynonymDone :: Either SomeException Api.StudyMaterial -> EventM Name AppState ()
handleSynonymDone (Right sm) = do
  st <- get
  let st' = applyAddedSynonyms (Api.smSubjectId sm) (Api.smMeaningSynonyms sm)
              (Just (Api.smId sm)) st
  case currentQuestion st' of
    Just q  -> put ((advanceOverride q st') { stNotice = Just "✓ synonym added", stError = Nothing })
    Nothing -> put st' { stMode = Normal, stNotice = Just "✓ synonym added", stError = Nothing }
  vScrollToBeginning (viewportScroll MainViewport)
handleSynonymDone (Left e) = do
  st <- get
  let (inp, acc) = case stMode st of
                     SynonymSubmitting p -> p
                     _                   -> (T.empty, [])
  put st { stMode  = WrongAnswer inp acc
         , stError = Just (T.pack ("synonym add failed: " <> shortErr e))
         }

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
  case handleSharedCtrlKeys refreshFn ev of
    Just action -> action
    Nothing -> case ev of
      V.EvKey (V.KChar 'q') [V.MCtrl] ->
        halt

      V.EvKey V.KEsc [] ->
        halt

      V.EvKey V.KEnter [] -> do
        st <- get
        case currentQuestion st of
          Nothing -> pure ()
          Just q  ->
            let ans = T.strip (stInput st)
            in if T.null ans then pure () else put (submitAnswer q ans st) >> resetMainScroll

      V.EvKey k [] | k `elem` [V.KBS, V.KDel] -> do
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
          , stNotice = Nothing
          }

      _ -> pure ()
  where
    resetMainScroll = vScrollToBeginning (viewportScroll MainViewport)

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
