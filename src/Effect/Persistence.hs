module Effect.Persistence (readData, writeData, backupData) where

import Control.Arrow ((>>>))
import Control.Exception (IOException, bracketOnError)
import Data.ByteString (ByteString, hPutStr, readFile)
import Data.Time.Clock (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import System.Directory
    ( XdgDirectory (..)
    , createDirectoryIfMissing
    , doesFileExist
    , getXdgDirectory
    , removeFile
    , renameFile
    )
import System.FilePath ((<.>), (</>))
import System.IO (hClose, openBinaryTempFile)

import Common
import Common.Prelude hiding (readFile)
import Effect.Error

readData :: (MonadEffectError e m, MonadIO m) => m ByteString
readData =
    getDataDirectory
        >>= ((</> dataFileName) >>> readFile >>> liftSafeIO @IOException)
        >>= liftEitherWith ReadFailed

writeData :: (MonadEffectError e m, MonadIO m) => ByteString -> m ()
writeData bs = do
    dir <- getDataDirectory
    result <- liftSafeIO @IOException $ bracketOnError
        (openBinaryTempFile dir $ dataFileName <.> "tmp")
        (liftA2 (*>) (hClose . snd) (removeFile . fst))
        \(tmp, h) -> do
            hPutStr h bs
            hClose h

            let
                fullPath = dir </> dataFileName

            exists <- doesFileExist fullPath

            when exists $ removeFile fullPath
            renameFile tmp fullPath

    liftEitherWith WriteFailed result

backupData :: (MonadEffectError e m, MonadIO m) => UTCTime -> m ()
backupData now = do
    dir <- getDataDirectory

    let
        oldFilePath = dir </> dataFileName
        newFilePath = dir </> nowStr <> "_" <> dataFileName <.> "bak"

    result <- liftSafeIO @IOException $ renameFile oldFilePath newFilePath

    liftEitherAs BackupFailed result
  where
    nowStr :: String
    nowStr = map (replaceToDot ":-") $ iso8601Show now

    replaceToDot :: String -> Char -> Char
    replaceToDot targets = bool '.' <$> id <*> (`notElem` targets)

getDataDirectory :: (MonadEffectError e m, MonadIO m) => m FilePath
getDataDirectory =
    liftSafeIO @IOException
        (getXdgDirectory XdgData dataDirectoryName >! createDirectoryIfMissing True)
        >>= liftEitherAs GettingDataDirectoryFailed

dataDirectoryName :: String
dataDirectoryName = "todo"

dataFileName :: String
dataFileName = "todo.dat"
