{-# LANGUAGE OverloadedStrings #-}

module Config
  ( KrokiConfig(..)
  , loadConfig
  , parseConfig
  , initConfig
  , defaultBatchSize
  , defaultRequeueAfter
  ) where

import Control.Exception (IOException, catch)
import Data.Char (isSpace, toLower)
import System.Directory (getXdgDirectory, XdgDirectory(XdgConfig), createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)

-- | Shared default values, used both in Main and in the init wizard.
defaultBatchSize :: Int
defaultBatchSize = 10

defaultRequeueAfter :: Int
defaultRequeueAfter = 7

data KrokiConfig = KrokiConfig
  { cfgToken :: Maybe String
  , cfgBatchSize :: Maybe Int
  , cfgRequeueAfter :: Maybe Int
  , cfgAudioPlayer :: Maybe String
  , cfgAudioAutoplay :: Maybe Bool
  } deriving (Show, Eq)

-- Loads ~/.config/kroki/config (via XDG)
loadConfig :: IO KrokiConfig
loadConfig = do
  base <- getXdgDirectory XdgConfig "kroki"
  let path = base </> "config"
  content <- readFile path `catch` \(_ :: IOException) -> pure ""
  pure $ parseConfig content

parseConfig :: String -> KrokiConfig
parseConfig s =
  KrokiConfig
    { cfgToken        = lookupKey "token"        ls
    , cfgBatchSize    = lookupInt "batch_size"   ls
    , cfgRequeueAfter = lookupInt "requeue_after" ls
    , cfgAudioPlayer  = lookupKey "audio_player" ls
    , cfgAudioAutoplay = lookupBool "audio_autoplay" ls
    }
  where ls = lines s

lookupKey :: String -> [String] -> Maybe String
lookupKey key ls =
  case [ val | line <- ls
             , let line' = trim line
             , not (null line')
             , head line' /= '#'
             , (k, rest) <- [break (=='=') line']
             , trim k == key
             , let val = trim (drop 1 rest)
             , not (null val)
             ] of
    (v:_) -> Just v
    []    -> Nothing

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- | Interactively create (or overwrite) ~/.config/kroki/config.
-- Prompts for each value; pressing Enter accepts the shown default.
initConfig :: IO ()
initConfig = do
  base <- getXdgDirectory XdgConfig "kroki"
  let path = base </> "config"

  existing   <- loadConfig
  fileExists <- doesFileExist path

  if fileExists
    then do
      putStrLn ("Config file already exists at: " <> path)
      putStr "Overwrite? [y/N] "
      hFlush stdout
      answer <- getLine
      if map toLower answer `elem` ["y", "yes"]
        then writeConfigInteractive base path existing
        else putStrLn "Aborted."
    else do
      putStrLn ("Creating config at: " <> path)
      writeConfigInteractive base path existing

writeConfigInteractive :: FilePath -> FilePath -> KrokiConfig -> IO ()
writeConfigInteractive dir path existing = do
  token      <- promptToken (cfgToken existing)
  batchSize  <- prompt "Batch size (0 = all available)" (Just (maybe (show defaultBatchSize)  show (cfgBatchSize existing)))
  requeueAft <- prompt "Requeue after (positions)" (Just (maybe (show defaultRequeueAfter) show (cfgRequeueAfter existing)))
  audioPlay  <- prompt "Audio player command (leave empty to disable)" (cfgAudioPlayer existing)
  autoplay   <- prompt "Auto-play reading audio on first appearance of a vocab reading question? [y/N]"
                  (Just (if cfgAudioAutoplay existing == Just True then "y" else "n"))

  let lineFor key val = key <> "=" <> val
      autoplayOn = map toLower autoplay `elem` ["y", "yes"]
      content = unlines $ concat
        [ [lineFor "token"        token]
        , [lineFor "batch_size"    batchSize  | not (null batchSize)]
        , [lineFor "requeue_after" requeueAft | not (null requeueAft)]
        , [lineFor "audio_player"  audioPlay  | not (null audioPlay)]
        , [lineFor "audio_autoplay" "true"    | autoplayOn]
        ]

  createDirectoryIfMissing True dir
  writeFile path content
  putStrLn ("Config written to: " <> path)

-- | Prompt for the API token. Unlike other fields, an existing token is
-- never echoed back (a secret shouldn't be shown on screen unnecessarily) --
-- pressing Enter just keeps it as-is. If there's no existing token to fall
-- back on, an empty answer would silently write an unusable config, so
-- re-prompt instead of accepting it.
promptToken :: Maybe String -> IO String
promptToken existing = do
  let hint = case existing of
               Just _  -> " [keep existing]"
               Nothing -> ""
  putStr ("WaniKani API token (required)" <> hint <> ": ")
  hFlush stdout
  input <- getLine
  case (trim input, existing) of
    ("", Just t)  -> pure t
    ("", Nothing) -> do
      putStrLn "A token is required."
      promptToken existing
    (v, _)        -> pure v

-- | Prompt the user for a value. Shows the default in brackets; Enter accepts it.
prompt :: String -> Maybe String -> IO String
prompt label mDefault = do
  let defStr = maybe "" (\d -> " [" <> d <> "]") mDefault
  putStr (label <> defStr <> ": ")
  hFlush stdout
  input <- getLine
  pure $ case (trim input, mDefault) of
    ("", Just d) -> d
    ("", Nothing) -> ""
    (v,  _)      -> v

lookupInt :: String -> [String] -> Maybe Int
lookupInt key ls =
  case lookupKey key ls of
    Just v  -> case reads v of
                 [(n, "")] -> Just n
                 _         -> Nothing
    Nothing -> Nothing

lookupBool :: String -> [String] -> Maybe Bool
lookupBool key ls =
  case map toLower <$> lookupKey key ls of
    Just v | v `elem` ["true", "1", "yes"] -> Just True
           | v `elem` ["false", "0", "no"] -> Just False
    _ -> Nothing
