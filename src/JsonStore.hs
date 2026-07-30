{-# LANGUAGE ScopedTypeVariables #-}

-- | Shared persistence for minaosou's small JSON state files in the XDG config
-- directory (@leeches.json@, @pending_reviews.json@). Both follow the same
-- rules: never crash a study session over local state, and never lose state
-- that was already on disk.
module JsonStore
  ( configFilePath
  , loadJsonFile
  , saveJsonFile
  ) where

import Control.Exception (IOException, catch)
import Data.Aeson (FromJSON, ToJSON, decodeStrict', encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import System.Directory
  ( XdgDirectory(XdgConfig)
  , createDirectoryIfMissing
  , getXdgDirectory
  , renameFile
  )
import System.FilePath ((</>), takeDirectory)

-- | Path to a state file in minaosou's XDG config directory.
configFilePath :: FilePath -> IO FilePath
configFilePath name = do
  base <- getXdgDirectory XdgConfig "minaosou"
  pure (base </> name)

-- | Read and decode a JSON state file, soft-failing to @fallback@ on a
-- missing, unreadable, or corrupt file (matching 'Config.loadConfig'\'s
-- convention -- local state must never crash a session).
--
-- The read is deliberately __strict__. With a lazy 'BL.readFile' the file
-- handle stays open and unread until the decoded value is forced, and these
-- files get rewritten in place moments later -- so a lazy read that had not
-- been forced yet would be served from the file /after/ it had already been
-- truncated for writing, silently yielding empty or partial data. That
-- exact interaction lost pending reviews: merging a new failure into a
-- non-empty @pending_reviews.json@ dropped entries instead of appending.
loadJsonFile :: FromJSON a => a -> FilePath -> IO a
loadJsonFile fallback path = do
  content <- BS.readFile path `catch` \(_ :: IOException) -> pure BS.empty
  pure (maybe fallback id (decodeStrict' content))

-- | Encode and write a JSON state file. Best-effort: a failure here must
-- never crash the caller.
--
-- The write goes to a sibling temp file which is then renamed over the
-- target, so the previous contents survive intact unless the new ones were
-- written in full. Writing in place would truncate the file on open, i.e.
-- destroy the old state before knowing the new state can be written.
saveJsonFile :: ToJSON a => FilePath -> a -> IO ()
saveJsonFile path value =
  ( do
      createDirectoryIfMissing True (takeDirectory path)
      let tmp = path <> ".tmp"
      BL.writeFile tmp (encode value)
      renameFile tmp path
  ) `catch` \(_ :: IOException) -> pure ()
