module Cli
  ( Options(..)
  , Command(..)
  , StudyOpts(..)
  , LeechesOpts(..)
  , parseCli
  ) where

import Options.Applicative
import Data.Version (showVersion)
import Paths_minaosou (version)

data StudyOpts = StudyOpts
  { studyBatchSize    :: Maybe Int
  , studyRequeueAfter :: Maybe Int
  } deriving (Show, Eq)

newtype LeechesOpts = LeechesOpts
  { leechesStudy :: Bool
  } deriving (Show, Eq)

data Command
  = WhoAmI
  | Reviews
  | Study StudyOpts
  | Leeches LeechesOpts
  | Init
  deriving (Show, Eq)

data Options = Options
  { optToken   :: Maybe String
  , optCommand :: Command
  } deriving (Show, Eq)

parseCli :: IO Options
parseCli = customExecParser cliPrefs parserInfo

-- | @helpShowGlobals@ makes @minaosou <command> --help@ also list the global
-- options (e.g. @--token@), so every option a command accepts is shown.
cliPrefs :: ParserPrefs
cliPrefs = prefs helpShowGlobals

parserInfo :: ParserInfo Options
parserInfo =
  info
    (optionsParser <**> helper <**> versioner)
    ( fullDesc
   <> progDesc "minaosou: tiny WaniKani CLI"
   <> header "minaosou" )

-- | @--version@, reporting the version cabal built this binary from
-- ('Paths_minaosou'), so a release binary can be told from a local build
-- without guessing.
versioner :: Parser (a -> a)
versioner = simpleVersioner ("minaosou " <> showVersion version)

optionsParser :: Parser Options
optionsParser =
  Options
    <$> optional tokenOption
    <*> commandParser

tokenOption :: Parser String
tokenOption =
  strOption
    ( long "token"
   <> metavar "TOKEN"
   <> help "WaniKani API token (overrides WANIKANI_API_TOKEN env var and config file)" )

batchSizeOption :: Parser Int
batchSizeOption =
  option auto
    ( long "batch-size"
   <> metavar "N"
   <> help "Max reviews per batch (0 = all available; overrides config batch_size)" )

requeueAfterOption :: Parser Int
requeueAfterOption =
  option auto
    ( long "requeue-after"
   <> metavar "K"
   <> help "Requeue a missed question K positions later (overrides config requeue_after)" )

-- | @hsubparser@ attaches a @--help@ option to every command, so each
-- command's options are listed by @minaosou <command> --help@.
commandParser :: Parser Command
commandParser =
  hsubparser
    (  command "whoami"
         (info (pure WhoAmI) (progDesc "Show current WaniKani user"))
    <> command "reviews"
         (info (pure Reviews) (progDesc "Show review schedule for the next 24 hours"))
    <> command "study"
         (info studyParser   (progDesc "Start a review batch (max N items)"))
    <> command "leeches"
         (info leechesParser (progDesc "List subjects tracked as leeches (repeated wrong answers across sessions)"))
    <> command "init"
         (info (pure Init)   (progDesc "Create or overwrite ~/.config/minaosou/config interactively"))
    )
  <|> pure (Study (StudyOpts Nothing Nothing))

studyParser :: Parser Command
studyParser =
  fmap Study $
    StudyOpts
      <$> optional batchSizeOption
      <*> optional requeueAfterOption

leechesParser :: Parser Command
leechesParser =
  fmap Leeches $
    LeechesOpts
      <$> switch
            ( long "study"
           <> help "Interactively practice all tracked leeches; never submitted to WaniKani, only updates the local leech tracker" )
