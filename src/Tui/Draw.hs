{-# LANGUAGE OverloadedStrings #-}

module Tui.Draw
  ( drawUi
  , theMap
  ) where

import qualified Api
import Tui.State
import Util (strPadLeft, strPadRight, wrapTextWidth)

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V
import Lens.Micro ((^.))

import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate, sortOn, elemIndex)
import Data.Time (NominalDiffTime, utcToLocalTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (L.listAttr,         V.defAttr)
  , (L.listSelectedAttr, V.defAttr `V.withStyle` V.reverseVideo)
  , (attrName "header",  fg V.cyan)
  , (attrName "ok",      fg V.green)
  , (attrName "bad",     fg V.red)
  , (attrName "hint",    V.defAttr `V.withStyle` V.dim)
  , (attrName "bigchar", V.defAttr `V.withStyle` V.bold `V.withForeColor` V.brightYellow)
  , (attrName "input",   V.defAttr `V.withForeColor` V.brightWhite)
  , (attrName "timer",   V.defAttr `V.withStyle` V.dim)
  ]

-- Widths of the two panes; 'sessionWidth' is the total the session UI
-- occupies, which the timer row is right-aligned within.
queueWidth, mainWidth, sessionWidth :: Int
queueWidth   = 28
mainWidth    = 80
sessionWidth = queueWidth + 1 + 1 + mainWidth   -- panes + vBorder + gap

drawUi :: AppState -> [Widget Name]
drawUi st =
  [ C.center $
      hLimit sessionWidth $
        vBox
          [ drawSessionTimer st
          , hBox
              [ hLimit queueWidth $ drawQueue st
              , B.vBorder
              , padLeft (Pad 1) $ hLimit mainWidth $ drawMain st
              ]
          ]
  ]

-- | Elapsed session time, in the top-right corner above the panes. Drawn as
-- part of the layout rather than as an overlaying layer: a layer positioned
-- over the corner covers the box border underneath it.
drawSessionTimer :: AppState -> Widget Name
drawSessionTimer st =
  padLeft Max $
    withAttr (attrName "timer") $
      str (elapsedLabel st)

drawQueue :: AppState -> Widget Name
drawQueue st =
  B.borderWithLabel (str "Queue") $
    vBox
      [ L.renderList drawQueueItem True (stQueueWidget st)
      , padTop (Pad 1) $
          withAttr (attrName "hint") $
            str ("remaining: " <> show (length (stQueue st)))
      ]

drawQueueItem :: Bool -> Q -> Widget Name
drawQueueItem sel q =
  let s = qSubject q
      w = str (displayItem s <> " [" <> kindLabel (qKind q) <> "]")
  in if sel then w else withAttr (attrName "hint") w

drawMain :: AppState -> Widget Name
drawMain st
  | stOverlay st == AllInfo =
      case infoQuestion st of
        Just q  -> drawAllInfo q st
        Nothing -> emptyWidget
  | stOverlay st == UserInfo       = drawUserInfo st
  | stOverlay st == ReviewSchedule = drawReviewSchedule st
  | otherwise =
  case currentQuestion st of
    Nothing ->
      B.borderWithLabel (str "Done") $
        viewport DoneViewport Vertical $
          padAll 1 $
            let confirmWidgets =
                  case stMode st of
                    ConfirmSubmit -> [padTop (Pad 1) (drawConfirmSubmit st)]
                    _             -> []
                detailWidgets =
                  case stSubmitDetails st of
                    [] -> []
                    ds -> padTop (Pad 1) (withAttr (attrName "hint")
                            (str (if stPracticeOnly st then "--- practice recorded ---" else "--- submitted ---")))
                        : map drawSubmitDetail ds
                bannerWidgets =
                  case stBanner st of
                    Just msg -> [padTop (Pad 1) (txt msg)]
                    Nothing  -> []
                errorWidgets =
                  case stError st of
                    Just msg -> [padTop (Pad 1) (withAttr (attrName "bad") (wideTxtWrap msg))]
                    Nothing  -> []
                noticeWidgets =
                  case stNotice st of
                    Just msg -> [padTop (Pad 1) (withAttr (attrName "ok") (wideTxtWrap msg))]
                    Nothing  -> []
                breakdownWidgets = drawBreakdown st
                hintLine =
                  case stMode st of
                    ConfirmSubmit ->
                      hintBox ["y/Enter=confirm", "n/Esc=cancel"]
                    Submitting ->
                      hintBox ["please wait…"]
                    _ | Just _ <- stBanner st ->
                          hintBox $ ["Esc=quit", "Ctrl-u=user", "Ctrl-v=reviews"] ++
                            [ "Ctrl-n=next batch" | stHasMore st ] ++
                            [ "↑↓/j/k=scroll" | not (null (stSubmitDetails st)) ]
                    _ -> hintBox $
                           [ if stPracticeOnly st then "Ctrl-s=finish" else "Ctrl-s=submit to WaniKani"
                           , "Esc=quit", "Ctrl-u=user", "Ctrl-v=reviews"
                           ]
            in vBox
                 ( [ withAttr (attrName "ok") $ str "Session finished."
                   , str ("correct:     " <> show (stCorrect st))
                   , str ("wrong:       " <> show (stWrong st))
                   , str ("overridden:  " <> show (stOverridden st))
                   , str ("submissions: " <> show (length (mkSubmissions st)))
                   ]
                -- No "session:" line: the corner timer stops at the last
                -- answer, so it is already showing the session total here.
                ++ [ str ("avg/item:    " <> avg) | Just avg <- [sessionAvgPerItem st] ]
                ++ breakdownWidgets
                ++ confirmWidgets
                ++ detailWidgets
                ++ bannerWidgets
                ++ errorWidgets
                ++ noticeWidgets
                ++ [ padTop (Pad 1) hintLine ]
                 )

    Just q ->
      B.borderWithLabel (str ("Current" <> srsIndicator q st)) $
        viewport MainViewport Vertical $
        padAll 1 $
          vBox $
            [ hBox
                [ withAttr (attrName "bigchar") $
                    txt (T.pack (displayCore (qSubject q)))
                , withAttr (attrName "header") $
                    txt (T.pack (displayTag (qSubject q) <> " — " <> kindLabel (qKind q)))
                ]
            , padTop (Pad 1) $
                withAttr (attrName "input") $
                  B.borderWithLabel (str "Input") $
                    padAll 1 $
                      txt (displayInput (qKind q) (stInput st))
            , padTop (Pad 1) $
                drawMode st q
            ]
            ++ ( case stError st of
                   Just msg ->
                     [ padTop (Pad 1)
                         (withAttr (attrName "bad") (wideTxtWrap msg))
                     ]
                   Nothing -> []
               )
            ++ ( case stNotice st of
                   Just msg ->
                     [ padTop (Pad 1)
                         (withAttr (attrName "ok") (wideTxtWrap msg))
                     ]
                   Nothing -> []
               )
            ++
            [ padTop (Pad 1) $
                withAttr (attrName "hint") $
                  normalHintWidget q st
            ]

drawMode :: AppState -> Q -> Widget Name
drawMode st q =
  case stMode st of
    Normal ->
      emptyWidget

    WrongAnswer input expected ->
      let shownInput = case qKind q of
            QReading -> normReading input
            QMeaning -> input
          mnemonic = case qKind q of
            QMeaning -> Api.subjMeaningMnemonic (qSubject q)
            QReading -> Api.subjReadingMnemonic (qSubject q)
          (compLabel, compHints)
            | Api.subjType (qSubject q) `elem` [Api.Vocabulary, Api.KanaVocabulary]
            = case qKind q of
                QReading -> ("Kanji: ", componentKanjiReadings st (qSubject q))
                QMeaning -> ("Kanji: ", componentKanjiMeanings st (qSubject q))
            | Api.subjType (qSubject q) == Api.Kanji, QMeaning <- qKind q
            = ("Radicals: ", componentRadicalMeanings st (qSubject q))
            | otherwise = ("", [])
      in vBox $
        [ withAttr (attrName "bad") $
            wideTxtWrap ("✗ you entered: " <> shownInput)
        , withAttr (attrName "ok") $
            wideTxtWrap ("✓ accepted:    " <> T.pack (intercalate ", " expected))
        ]
        ++ [ padTop (Pad 1) $ withAttr (attrName "bad") $ wideTxtWrap ("⚠ " <> c)
           | Just c <- [confusionHint st q input]
           ]
        ++ [ padTop (Pad 1) $
               wideTxtWrap (compLabel <> T.intercalate "  ·  " compHints)
           | not (null compHints)
           ]
        ++ [ padTop (Pad 1) $ wideTxtWrap (stripWkTags m)
           | Just m <- [mnemonic]
           ]

    Feedback msg ->
      withAttr (attrName "ok") $ txt msg

    SynonymEntry buf _ ->
      vBox
        [ withAttr (attrName "header") $
            txt "Add meaning synonym to WaniKani:"
        , padTop (Pad 1) $
            withAttr (attrName "input") $
              B.borderWithLabel (str "Synonym") $
                padAll 1 $ txt (if T.null buf then " " else buf)
        ]

    SynonymSubmitting _ ->
      withAttr (attrName "header") $ txt "Adding synonym…"

    _ -> emptyWidget

-- | If the current subject is a kanji and the wrong answer given actually
-- matches one of its visually-similar kanji, name the mix-up explicitly
-- instead of leaving the user to hunt for it via Ctrl-a.
confusionHint :: AppState -> Q -> Text -> Maybe Text
confusionHint st q input =
  let subj = qSubject q
  in if Api.subjType subj /= Api.Kanji
       then Nothing
       else
         let sims = lookupSubjects st (Api.subjVisuallySimilarIds subj)
             matches = case qKind q of
               QMeaning ->
                 [ s | s <- sims, normMeaning input `elem` map normMeaning (Api.subjMeanings s) ]
               QReading ->
                 [ s | s <- sims, normReading input `elem` map normReading (acceptedReadings s) ]
         in case matches of
              (s:_) -> Just $
                "Possible mix-up: this looks like " <> T.pack (displayCore s)
                <> " (" <> T.intercalate ", " (Api.subjMeanings s) <> "), not "
                <> T.pack (displayCore subj)
              [] -> Nothing

-- | The subjects referenced by a list of subject IDs that are actually
-- present in the loaded subject map (missing/never-fetched ids are dropped).
lookupSubjects :: AppState -> [Api.SubjectId] -> [Api.Subject]
lookupSubjects st = mapMaybe (\sid -> M.lookup sid (stAllSubjects st))

-- | Component breakdown for a wrong-answer explanation, formatted as
-- "字 (info, info)" per component — helps explain a wrong answer by showing
-- how it's built from its components. Used for a vocabulary subject's
-- component kanji (readings on a reading question, meanings on a meaning
-- question) and a kanji subject's component radicals (meanings only).
componentInfo :: (Api.Subject -> Bool) -> (Api.Subject -> [Text]) -> AppState -> Api.Subject -> [Text]
componentInfo typeFilter fieldFn st subj =
  [ fromMaybe "?" (Api.subjChars c) <> " (" <> T.intercalate ", " fs <> ")"
  | c <- lookupSubjects st (Api.subjComponentIds subj)
  , typeFilter c
  , let fs = fieldFn c
  , not (null fs)
  ]

componentKanjiReadings :: AppState -> Api.Subject -> [Text]
componentKanjiReadings = componentInfo (\c -> Api.subjType c == Api.Kanji) acceptedReadings

componentKanjiMeanings :: AppState -> Api.Subject -> [Text]
componentKanjiMeanings = componentInfo (\c -> Api.subjType c == Api.Kanji) Api.subjMeanings

componentRadicalMeanings :: AppState -> Api.Subject -> [Text]
componentRadicalMeanings = componentInfo (\c -> Api.subjType c == Api.Radical) Api.subjMeanings

-- | A submitted-review detail line, e.g. "字 (word)  incorrect (m:1 r:0) →
-- Guru" — highlighted in the "bad" attr when it was answered incorrectly, so
-- leeches stand out in the post-submission list rather than blending into
-- the rest of the (all dim) detail lines.
drawSubmitDetail :: String -> Widget Name
drawSubmitDetail d
  | "incorrect" `T.isInfixOf` T.pack d = withAttr (attrName "bad")  $ wideTxtWrap (T.pack d)
  | otherwise                          = withAttr (attrName "hint") $ wideTxtWrap (T.pack d)

drawConfirmSubmit :: AppState -> Widget Name
drawConfirmSubmit st =
  let subs = mkSubmissions st
      total = length subs
      withMistakes = length [ () | s <- subs, subWrongMeaning s > 0 || subWrongReading s > 0 ]
      prompt
        | stPracticeOnly st =
            "Finish practice round of " <> show total <> " items? [y/N]"
        | otherwise =
            "Submit " <> show total <> " reviews to WaniKani? [y/N]"
  in vBox
      [ withAttr (attrName "header") $
          str prompt
      , padTop (Pad 1) $
          str ("Items with mistakes: " <> show withMistakes)
      ]

drawAllInfo :: Q -> AppState -> Widget Name
drawAllInfo q st =
  B.borderWithLabel (hBox [withAttr (attrName "bigchar") (txt coreLabel), txt tagLabel]) $
    viewport InfoViewport Vertical $
      padAll 1 $
        vBox $
             assignSection
          ++ compSection
          ++ amalgSection
          ++ similarSection
          ++ [ str ("Meanings:  " <> T.unpack (T.intercalate ", " (Api.subjMeanings subj))) ]
          ++ readSection
          ++ mnSection "Meaning mnemonic" (Api.subjMeaningMnemonic subj)
          ++ mnSection "Reading mnemonic" (Api.subjReadingMnemonic subj)
          ++ [ padTop (Pad 1) $
                 hintBox ["Ctrl-a/Esc/Enter=close", "↑↓/j/k=scroll"] ]
  where
    subj      = qSubject q
    coreLabel = fromMaybe "?" (Api.subjChars subj)
    tagLabel  = " · " <> subjTypeLabel (Api.subjType subj)

    assignSection =
      let stageStr = case M.lookup (Api.subjId subj) (stSubjToAsg st) of
                       Just asg -> Api.srsStageLabel (Api.asSrsStage asg)
                       Nothing  -> "?"
          missedLine = M.lookup (Api.subjId subj) (stPriorWrong st) >>= missedBeforeLabel
      in [ str ("Level:     " <> show (Api.subjLevel subj))
         , str ("SRS stage: " <> stageStr)
         ]
      ++ [ str ("Missed before: " <> l) | Just l <- [missedLine] ]
      ++ [ str "" ]

    -- A component kanji is expanded (readings, radical composition) only
    -- when the subject on screen is a vocabulary item; for a kanji subject
    -- the components already /are/ its radicals, which need no breakdown.
    expandKanji c =
      Api.subjType c == Api.Kanji
      && (Api.subjType subj == Api.Vocabulary || Api.subjType subj == Api.KanaVocabulary)

    showKanjiReadings c = expandKanji c && not (null (Api.subjReadings c))

    -- The radicals a component kanji is built from, as "glyph name". A few
    -- WaniKani radicals are image-only (no characters at all) -- those show
    -- the name on its own rather than a placeholder.
    kanjiRadicals c
      | not (expandKanji c) = []
      | otherwise =
          [ maybe m (<> " " <> m) (Api.subjChars r)
          | r <- lookupSubjects st (Api.subjComponentIds c)
          , Api.subjType r == Api.Radical
          , m <- take 1 (Api.subjMeanings r)
          ]

    renderComponent c =
      let chars   = T.unpack (fromMaybe "?" (Api.subjChars c))
          meanings = T.unpack (T.intercalate ", " (Api.subjMeanings c))
          headerW  = str ("  " <> chars <> "  " <> meanings)
          detail label vals =
            [ withAttr (attrName "hint") $
                wideTxtWrap ("       " <> label <> ": " <> T.intercalate ", " vals)
            | not (null vals)
            ]
      in [ headerW ]
         ++ detail "readings" [ r | showKanjiReadings c, r <- Api.subjReadings c ]
         ++ detail "radicals" (kanjiRadicals c)

    compSection =
      let comps = lookupSubjects st (Api.subjComponentIds subj)
      in case comps of
           [] -> []
           cs -> str "Components:" : concatMap renderComponent cs ++ [str ""]

    renderAmalgamation v =
      let chars   = T.unpack (fromMaybe "?" (Api.subjChars v))
          meaning = case Api.subjMeanings v of
                      (m:_) -> T.unpack m
                      []    -> "?"
          rd      = case Api.subjReadings v of
                      (r:_) -> " (" <> T.unpack r <> ")"
                      []    -> ""
      in str ("  " <> chars <> rd <> "  " <> meaning)

    amalgSection =
      case Api.subjType subj of
        Api.Kanji ->
          let vocabs = lookupSubjects st (Api.subjAmalgamationIds subj)
          in case vocabs of
               [] -> []
               vs -> str "Vocabulary:" : map renderAmalgamation vs ++ [str ""]
        _ -> []

    similarSection =
      case Api.subjType subj of
        Api.Kanji ->
          let sims = lookupSubjects st (Api.subjVisuallySimilarIds subj)
          in case sims of
               [] -> []
               vs -> str "Visually similar:" : map renderAmalgamation vs ++ [str ""]
        _ -> []

    readSection =
      case Api.subjReadings subj of
        [] -> []
        rs -> [ str ("Readings:  " <> T.unpack (T.intercalate ", " rs)) ]

    mnSection _     Nothing  = []
    mnSection title (Just t) =
      [ str ""
      , withAttr (attrName "hint") (str (title <> ":"))
      , wideTxtWrap (stripWkTags t)
      ]

drawUserInfo :: AppState -> Widget Name
drawUserInfo st =
  let u = stUser st
  in B.borderWithLabel (str "User") $
       viewport UserViewport Vertical $
         padAll 1 $
           vBox
             [ txt ("Username: " <> Api.userUsername u)
             , str ("Level:    " <> show (Api.userLevel u))
             , txt ("Profile:  " <> Api.userProfileUrl u)
             , padTop (Pad 1) $ hintBox ["Ctrl-u/Esc=close"]
             ]

drawReviewSchedule :: AppState -> Widget Name
drawReviewSchedule st =
  let rows    = Api.reviewsPerHourNext24 (stNow st) (stSummary st)
      nowAvail = Api.reviewsAvailableNow (stNow st) (stSummary st)
      fmtHour utc =
        formatTime defaultTimeLocale "%F %H:00" (utcToLocalTime (stTZ st) utc)
  in B.borderWithLabel (str "Review Schedule") $
       viewport ReviewViewport Vertical $
         padAll 1 $
           vBox $
             [ withAttr (attrName "header") $
                 str ("Available now: " <> show nowAvail)
             , padTop (Pad 1) $
                 withAttr (attrName "hint") $ str "Hour (local)            New   Open"
             ] ++
             map (\(hStart, newN, openN) ->
               str ( strPadRight 24 (fmtHour hStart)
                  <> strPadLeft 3 (show newN) <> "  "
                  <> strPadLeft 4 (show openN) )
             ) rows ++
             [ padTop (Pad 1) $ hintBox ["Ctrl-v/Esc=close", "↑↓/j/k=scroll"] ]

-- | Session-end breakdown: for each reviewed subject, whether it was
-- answered with zero mistakes ("clean") or missed at least once, grouped by
-- subject type and by (pre-review) SRS stage.
-- | Mean time per item over the whole session, or 'Nothing' before any item
-- has been answered.
sessionAvgPerItem :: AppState -> Maybe String
sessionAvgPerItem st =
  formatAvgPerItem (sum (M.elems (stSubjTime st))) (M.size (stSubjTime st))

drawBreakdown :: AppState -> [Widget Name]
drawBreakdown st =
  case perSubject of
    [] -> []
    _  -> [ padTop (Pad 1) $ withAttr (attrName "hint") $ str "--- by type ---" ]
       ++ map row (sortByOrder typeOrder byType)
       ++ [ padTop (Pad 1) $ withAttr (attrName "hint") $ str "--- by SRS stage ---" ]
       ++ map row (sortByOrder stageOrder byStage)
  where
    perSubject =
      [ (subj, missed, subjectTime st sid)
      | (sid, p) <- M.toList (stProgress st)
      , Just subj <- [M.lookup sid (stAllSubjects st)]
      , let missed = pMeaningWrong p > 0 || pReadingWrong p > 0
      ]

    -- Clean / missed / time / how many of them the session actually spent
    -- time on: an abandoned session leaves untouched subjects in the tally,
    -- and averaging over those would understate the time per item.
    tally :: (Api.Subject -> Text) -> [(Text, (Int, Int, NominalDiffTime, Int))]
    tally keyFn = M.toList $ M.fromListWith addTally
      [ (keyFn subj, if missed then (0, 1, t, timed) else (1, 0, t, timed))
      | (subj, missed, t) <- perSubject
      , let timed = if t > 0 then 1 else 0 ]
    addTally (c1, m1, t1, n1) (c2, m2, t2, n2) = (c1 + c2, m1 + m2, t1 + t2, n1 + n2)

    byType  = tally (subjTypeLabel . Api.subjType)
    byStage = tally stageOf
      where
        stageOf subj =
          case M.lookup (Api.subjId subj) (stSubjToAsg st) of
            Just asg -> T.pack (Api.srsStageLabel (Api.asSrsStage asg))
            Nothing  -> "?"

    typeOrder  = map subjTypeLabel [Api.Radical, Api.Kanji, Api.Vocabulary, Api.KanaVocabulary]
    stageOrder = map (T.pack . Api.srsStageLabel)
                     [Api.Initiate, Api.Apprentice, Api.Guru, Api.Master, Api.Enlightened, Api.Burned]

    sortByOrder order = sortOn (\(label, _) -> fromMaybe maxBound (elemIndex label order))

    row (label, (clean, missed, total, timed)) =
      str (strPadRight 16 (T.unpack label)
        <> strPadRight 10 ("clean: "  <> show clean)
        <> strPadRight 11 ("missed: " <> show missed)
        <> maybe "" ("avg: " <>) (formatAvgPerItem total timed))

subjTypeLabel :: Api.SubjectType -> Text
subjTypeLabel Api.Radical        = "Radical"
subjTypeLabel Api.Kanji          = "Kanji"
subjTypeLabel Api.Vocabulary     = "Vocabulary"
subjTypeLabel Api.KanaVocabulary = "Kana Vocabulary"

-- Strip WaniKani HTML-like tags (<radical>…</radical> etc.), keeping inner text.
stripWkTags :: Text -> Text
stripWkTags t = T.pack (go (T.unpack t))
  where
    go []        = []
    go ('<':cs)  = go (drop 1 (dropWhile (/= '>') cs))
    go (c  :cs)  = c : go cs

-- | Like Brick's 'txtWrap', but wraps by terminal /display/ width so lines
-- containing CJK (kanji/kana, rendered 2 cells wide) don't overflow the pane
-- and get clipped. Brick's 'txtWrap' measures by code-point count, which
-- undercounts every wide character; in a Japanese-learning app that means
-- essentially every wrapped feedback/mnemonic line was breaking a column or
-- two too late. Reads the available width from the render context so it
-- adapts to the current pane size.
wideTxtWrap :: Text -> Widget n
wideTxtWrap t =
  Widget Greedy Fixed $ do
    ctx <- getContext
    let w = ctx ^. availWidthL
    render (txt (T.intercalate "\n" (wrapTextWidth w t)))

-- | Render a list of hint strings as auto-wrapping text.
hintBox :: [Text] -> Widget Name
hintBox hints =
  withAttr (attrName "hint") $
    wideTxtWrap (T.intercalate "  " hints)

srsIndicator :: Q -> AppState -> String
srsIndicator q st =
  case M.lookup (Api.subjId (qSubject q)) (stSubjToAsg st) of
    Just asg -> " · " <> Api.srsStageLabel (Api.asSrsStage asg)
    Nothing  -> ""

normalHintWidget :: Q -> AppState -> Widget Name
normalHintWidget q st =
  case stMode st of
    WrongAnswer _ _ ->
      hintBox $
        [ "Ctrl-o=override correct", "Ctrl-r=requeue (no penalty)", "Enter=requeue (wrong)" ]
        ++ [ "Ctrl-y=add synonym" | qKind q == QMeaning ]
        ++ [ "Ctrl-a=all info", "Ctrl-u=user", "Ctrl-v=reviews", "↑↓=scroll" ]
        ++ [ "Ctrl-p=play audio" | hasAudio q st ]
    SynonymEntry _ _ ->
      hintBox [ "Enter=add to WaniKani", "Esc=cancel" ]
    SynonymSubmitting _ ->
      hintBox [ "please wait…" ]
    _ ->
      hintBox $
        [ "Enter=submit", "Ctrl-o=override", "Ctrl-r=requeue"
        , "Ctrl-a=all info", "Ctrl-u=user", "Ctrl-v=reviews", "Esc=quit"
        ] ++ [ "Ctrl-p=play audio" | hasAudio q st ]
