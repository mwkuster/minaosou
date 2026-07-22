{-# LANGUAGE ScopedTypeVariables #-}

module Util
  ( strPadLeft
  , strPadRight
  , shortErr
  , trySync
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
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Req as Req
import Network.HTTP.Types (statusCode, statusMessage)

-- | Pad a String on the left with spaces to at least n characters (right-aligns text).
strPadLeft :: Int -> String -> String
strPadLeft n s = replicate (max 0 (n - length s)) ' ' <> s

-- | Pad a String on the right with spaces to at least n characters (left-aligns text).
strPadRight :: Int -> String -> String
strPadRight n s = s <> replicate (max 0 (n - length s)) ' '

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
