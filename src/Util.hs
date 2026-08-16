{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util
  ( strPadLeft
  , strPadRight
  , groupDigits
  , median
  , shortErr
  , trySync
  , displayWidth
  , wrapTextWidth
  ) where

import Control.Exception
  ( SomeAsyncException
  , SomeException
  , displayException
  , fromException
  , throwIO
  , try
  )
import qualified Data.ByteString.Char8 as BS8
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.Text.Width (safeWcwidth)
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Req as Req
import Network.HTTP.Types (statusCode, statusMessage)

-- | Pad a String on the left with spaces to at least n characters (right-aligns text).
strPadLeft :: Int -> String -> String
strPadLeft n s = replicate (max 0 (n - length s)) ' ' <> s

-- | Pad a String on the right with spaces to at least n characters (left-aligns text).
strPadRight :: Int -> String -> String
strPadRight n s = s <> replicate (max 0 (n - length s)) ' '

-- | Thousands separators, so a five- or six-figure review count can be read
-- at a glance rather than counted digit by digit.
groupDigits :: Int -> String
groupDigits n
  | n < 0     = '-' : groupDigits (negate n)
  | otherwise = reverse (intercalate "," (chunksOf3 (reverse (show n))))
  where
    chunksOf3 [] = []
    chunksOf3 s  = let (a, b) = splitAt 3 s in a : chunksOf3 b

-- | Median of an already-sorted list, averaging the middle pair when the
-- count is even. 'Nothing' for an empty one, since there is no sensible
-- answer to report and a zero would read as a real measurement.
median :: [Double] -> Maybe Double
median [] = Nothing
median xs
  | odd n     = Just (xs !! mid)
  | otherwise = Just ((xs !! (mid - 1) + xs !! mid) / 2)
  where
    n   = length xs
    mid = n `div` 2

-- | Terminal display width of text: East-Asian wide characters (kanji, kana)
-- count as 2 cells, control characters as 0 -- matching how vty actually
-- renders them. Brick's built-in 'txtWrap' instead measures by 'T.length'
-- (one per code point), so a wrapped line containing CJK is under-measured
-- and overflows its pane, and the terminal clips the right edge -- the
-- classic "the line break is off by one (per wide char)" symptom.
displayWidth :: Text -> Int
displayWidth = T.foldl' (\acc c -> acc + safeWcwidth c) 0

-- | Greedy word-wrap to a maximum /display/ width (wide-char aware), for use
-- where Brick's 'txtWrap' would miscount CJK. Existing newlines are kept as
-- hard breaks (and blank lines preserved); within a paragraph, words are
-- packed by display width. A line that already fits is returned verbatim, so
-- deliberate alignment spacing on short lines is untouched. A single word
-- wider than the limit is left on its own line rather than dropped.
wrapTextWidth :: Int -> Text -> [Text]
wrapTextWidth limit = concatMap wrapParagraph . T.splitOn "\n"
  where
    w = max 1 limit
    wrapParagraph para
      | displayWidth para <= w = [para]
      | otherwise = case T.words para of
          []     -> [""]
          (x:xs) -> fill x xs
    fill cur [] = [cur]
    fill cur (y:ys)
      | displayWidth cur + 1 + displayWidth y <= w = fill (cur <> " " <> y) ys
      | otherwise                                  = cur : fill y ys

-- | One-line, human-readable summary of an exception, short enough to drop
-- into a TUI line or a log entry.
--
-- HTTP exceptions get explicit handling rather than a plain
-- 'displayException'. 'HC.HttpException'\'s 'Show' instance dumps the entire
-- 'HC.Request' record -- fifteen-odd lines of host/port/header boilerplate --
-- /before/ the 'HC.HttpExceptionContent' that says what actually went wrong.
-- Summarising by taking the first line (as this used to) therefore rendered
-- every network failure alike as the same constant, information-free string
-- @"VanillaHttpException (HttpExceptionRequest Request {"@, which made a 429
-- rate-limit indistinguishable from a dropped connection or a 422 rejection
-- in both the Done-screen details and the logs.
shortErr :: SomeException -> String
shortErr = truncateTo 200 . unwords . words . describe
  where
    truncateTo n s
      | length s > n = take (n - 3) s <> "..."
      | otherwise    = s

describe :: SomeException -> String
describe e
  | Just (Req.VanillaHttpException hc) <- fromException e = describeHttp hc
  | Just (Req.JsonHttpException msg)   <- fromException e = "malformed JSON in response: " <> msg
  | Just hc                            <- fromException e = describeHttp hc
  | otherwise                                             = displayException e

describeHttp :: HC.HttpException -> String
describeHttp (HC.InvalidUrlException url reason) = "invalid URL " <> url <> ": " <> reason
describeHttp (HC.HttpExceptionRequest _ content) = describeContent content

-- | 'HC.StatusCodeException' likewise buries the status behind a full
-- 'HC.Response' dump, so pull the code out explicitly -- it is the single
-- most useful thing to know here (429 = rate limited, 422 = WaniKani
-- rejected the review outright, 401 = bad token).
describeContent :: HC.HttpExceptionContent -> String
describeContent (HC.StatusCodeException resp body) =
  let st      = HC.responseStatus resp
      summary = "HTTP " <> show (statusCode st) <> " " <> BS8.unpack (statusMessage st)
  in if BS8.null body
       then summary
       else summary <> ": " <> BS8.unpack (BS8.take 200 body)
describeContent other = show other

-- | Like @'try' \@'SomeException'@, but never swallows an /asynchronous/
-- exception (thread cancellation, timeout, Ctrl-C) -- those are rethrown so
-- the thread can actually die.
--
-- This matters wherever a caught exception is turned into a data-level
-- failure: 'Control.Concurrent.Async.mapConcurrently' cancels its siblings
-- as soon as one action throws, and a plain @try@ would record each of those
-- cancellations as an ordinary "this review failed" outcome -- persisting it
-- for a retry of a request that was never actually attempted.
trySync :: IO a -> IO (Either SomeException a)
trySync act = do
  result <- try act
  case result of
    Left e | isAsync e -> throwIO e
    _                  -> pure result
  where
    isAsync e = case fromException e :: Maybe SomeAsyncException of
      Just _  -> True
      Nothing -> False
