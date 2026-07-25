{-# LANGUAGE OverloadedStrings #-}

module Romaji
  ( romajiToHiragana
  , romajiToHiraganaLive
  ) where

import Data.Char (isAsciiLower, isAsciiUpper)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T

-- | Convert (mostly Hepburn) romaji to hiragana.
-- Intended for WaniKani-style readings; not a full IME.
romajiToHiragana :: Text -> Text
romajiToHiragana = go . T.toLower . T.filter keep
  where
    keep c = isAsciiLower c || isAsciiUpper c || c == '\''
    go t
      | T.null t  = ""
      | otherwise =
          case smallTsu t of
            Just (out, rest) -> out <> go rest
            Nothing ->
              case parseN t of
                Just rest -> "ん" <> go rest
                Nothing  ->
                  case matchKana t of
                    Just (k, rest) -> k <> go rest
                    Nothing ->
                      -- If unknown chunk, drop one char (keeps session moving)
                      go (T.drop 1 t)

-- small っ for doubled consonants: kk, ss, tt, pp, etc.
-- not for vowels or 'n' (handled separately)
smallTsu :: Text -> Maybe (Text, Text)
smallTsu t =
  case T.uncons t of
    Just (c1, rest1) ->
      case T.uncons rest1 of
        Just (c2, _) ->
          let isConsonant c = c `elem` ("bcdfghjklmnpqrstvwxyz" :: String)
          in if c1 == c2 && isConsonant c1 && c1 /= 'n'
               then Just ("っ", rest1)   -- drop first of the doubled consonant
               else Nothing
        Nothing -> Nothing
    Nothing -> Nothing

-- Handle 'n' as ん when:
--  - "n'" explicitly (→ ん, both characters consumed)
--  - "nn" (→ ん, both n's consumed) -- matching wanakana / WaniKani. A vowel
--    that follows therefore stands on its own: "onna" → おんあ, "zennin" →
--    ぜんいん. To write ん immediately before a な-row syllable, separate them
--    with an apostrophe or a third n: "on'na" / "onnna" → おんな.
--  - a lone "n" before a non-vowel (and not 'y'), or at end of input
parseN :: Text -> Maybe Text
parseN t
  | "n'" `T.isPrefixOf` t = Just (T.drop 2 t)
  | "nn" `T.isPrefixOf` t = Just (T.drop 2 t)
  | "n"  `T.isPrefixOf` t =
      case T.uncons (T.drop 1 t) of
        Nothing     -> Just ""   -- trailing n
        Just (c, _) ->
          if c `elem` ("aiueoy" :: String)
            then Nothing               -- part of syllable: na/nya/ni...
            else Just (T.drop 1 t)     -- n + consonant => ん
  | otherwise = Nothing

matchKana :: Text -> Maybe (Text, Text)
matchKana t = do
  -- prefer longest match
  (r, k) <- find (\(r,_) -> r `T.isPrefixOf` t) table
  pure (k, T.drop (T.length r) t)

-- Ordered longest-first. Covers the bulk of WK readings.
table :: [(Text, Text)]
table =
  [ ("kya","きゃ"),("kyu","きゅ"),("kyo","きょ")
  , ("gya","ぎゃ"),("gyu","ぎゅ"),("gyo","ぎょ")
  , ("sha","しゃ"),("shu","しゅ"),("sho","しょ")
  , ("ja","じゃ"), ("ju","じゅ"), ("jo","じょ")
  , ("cha","ちゃ"),("chu","ちゅ"),("cho","ちょ")
  , ("nya","にゃ"),("nyu","にゅ"),("nyo","にょ")
  , ("hya","ひゃ"),("hyu","ひゅ"),("hyo","ひょ")
  , ("bya","びゃ"),("byu","びゅ"),("byo","びょ")
  , ("pya","ぴゃ"),("pyu","ぴゅ"),("pyo","ぴょ")
  , ("mya","みゃ"),("myu","みゅ"),("myo","みょ")
  , ("rya","りゃ"),("ryu","りゅ"),("ryo","りょ")

  , ("tsu","つ"),("shi","し"),("chi","ち"),("fu","ふ")
  , ("dzu","づ"),("du","づ"),("di","ぢ"),("ji","じ")   -- WK uses 'ji'; di/du are kana alternatives
  , ("kwi","くぃ"),("kwe","くぇ"),("kwo","くぉ")
  , ("gwi","ぐぃ"),("gwe","ぐぇ"),("gwo","ぐぉ")

  ]
  ++ basicRows

basicRows :: [(Text, Text)]
basicRows =
  [ ("a","あ"),("i","い"),("u","う"),("e","え"),("o","お")

  , ("ka","か"),("ki","き"),("ku","く"),("ke","け"),("ko","こ")
  , ("sa","さ"),("su","す"),("se","せ"),("so","そ") -- shi handled above
  , ("ta","た"),("te","て"),("to","と")             -- chi/tsu handled above
  , ("na","な"),("ni","に"),("nu","ぬ"),("ne","ね"),("no","の")
  , ("ha","は"),("hi","ひ"),("he","へ"),("ho","ほ") -- fu handled above
  , ("ma","ま"),("mi","み"),("mu","む"),("me","め"),("mo","も")
  , ("ya","や"),("yu","ゆ"),("yo","よ")
  , ("ra","ら"),("ri","り"),("ru","る"),("re","れ"),("ro","ろ")
  , ("wa","わ"),("wo","を")
  , ("ga","が"),("gi","ぎ"),("gu","ぐ"),("ge","げ"),("go","ご")
  , ("za","ざ"),("zu","ず"),("ze","ぜ"),("zo","ぞ")
  , ("da","だ"),("de","で"),("do","ど")
  , ("ba","ば"),("bi","び"),("bu","ぶ"),("be","べ"),("bo","ぼ")
  , ("pa","ぱ"),("pi","ぴ"),("pu","ぷ"),("pe","ぺ"),("po","ぽ")

  -- small vowels (occasionally useful)
  , ("xa","ぁ"),("xi","ぃ"),("xu","ぅ"),("xe","ぇ"),("xo","ぉ")
  , ("la","ぁ"),("li","ぃ"),("lu","ぅ"),("le","ぇ"),("lo","ぉ")

  -- small ya/yu/yo
  , ("xya","ゃ"),("xyu","ゅ"),("xyo","ょ")
  , ("lya","ゃ"),("lyu","ゅ"),("lyo","ょ")

  -- small tsu
  , ("xtsu","っ"),("ltsu","っ")
  ]

-- | Like 'romajiToHiragana' but for live display while the user is still typing.
-- Converts all complete syllables from the left; any trailing characters that
-- could still be part of an in-progress syllable are shown as-is.
-- Example: "shika" → "しか", "shi" → "し", "sh" → "sh" (pending)
romajiToHiraganaLive :: Text -> Text
romajiToHiraganaLive input =
  let t             = T.toLower (T.filter keep input)
      (kana, pend)  = liveConvert t
  in kana <> pend
  where
    keep c = isAsciiLower c || isAsciiUpper c || c == '\''

liveConvert :: Text -> (Text, Text)
liveConvert t
  | T.null t  = (T.empty, T.empty)
  | otherwise =
      case smallTsu t of
        Just (k, rest) -> prepend k (liveConvert rest)
        Nothing ->
          case liveParseN t of
            Just (k, rest) -> prepend k (liveConvert rest)
            Nothing ->
              case matchKana t of
                Just (k, rest) -> prepend k (liveConvert rest)
                Nothing ->
                  if isPending t
                    then (T.empty, t)
                    else prepend (T.take 1 t) (liveConvert (T.drop 1 t))
  where
    prepend k (k2, p) = (k <> k2, p)

-- | Like 'parseN' but keeps a trailing lone \"n\" pending instead of
-- converting it immediately, so the user can still type \"na\", \"ni\", etc.
liveParseN :: Text -> Maybe (Text, Text)
liveParseN t
  | "n'"  `T.isPrefixOf` t = Just ("ん", T.drop 2 t)
  | "nn"  `T.isPrefixOf` t = Just ("ん", T.drop 2 t)  -- both n's → ん (see 'parseN')
  | "n"   `T.isPrefixOf` t =
      case T.uncons (T.drop 1 t) of
        Nothing     -> Nothing   -- trailing n: keep as pending
        Just (c, _)
          | c `elem` ("aiueoy" :: String) -> Nothing   -- na/ni/nu/…
          | otherwise                     -> Just ("ん", T.drop 1 t)
  | otherwise = Nothing

-- | True when 't' is a non-empty prefix of at least one romaji entry in
-- the table, meaning more characters could complete it into a kana.
isPending :: Text -> Bool
isPending t =
  not (T.null t) && any (\(r, _) -> t `T.isPrefixOf` r) table
