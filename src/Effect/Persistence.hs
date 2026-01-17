module Effect.Persistence (readData, readConfig, writeData, backupData) where

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
import Text.Printf (printf)

import Common
import Effect.Error
import External.Prelude hiding (readFile)

readData :: (MonadEffectError e m, MonadIO m) => m ByteString
readData = readRaw getDataDirectory dataFileName

readConfig :: (MonadEffectError e m, MonadIO m) => m ByteString
readConfig = readRaw getConfigDirectory configFileName

writeData :: (MonadEffectError e m, MonadIO m) => ByteString -> m ()
writeData = writeRaw getDataDirectory dataFileName

backupData :: (MonadEffectError e m, MonadIO m) => UTCTime -> m ()
backupData = backupRaw getDataDirectory dataFileName

-- private

readRaw
    :: (MonadEffectError e m, MonadIO m) => m FilePath -> String -> m ByteString
readRaw mdir file = mdir >>= (`readRawAt` file)

writeRaw
    :: (MonadEffectError e m, MonadIO m) => m FilePath -> String -> ByteString -> m ()
writeRaw mdir file bs = mdir >>= \dir -> writeRawAt dir file bs

backupRaw
    :: (MonadEffectError e m, MonadIO m) => m FilePath -> String -> UTCTime -> m ()
backupRaw mdir file now = mdir >>= \dir -> backupRawAt dir file now

getDataDirectory :: (MonadEffectError e m, MonadIO m) => m FilePath
getDataDirectory = getDirectory XdgData directoryName GettingDataDirectoryFailed

getConfigDirectory :: (MonadEffectError e m, MonadIO m) => m FilePath
getConfigDirectory = getDirectory XdgConfig directoryName GettingConfigDirectoryFailed

readRawAt
    :: (MonadEffectError e m, MonadIO m) => FilePath -> String -> m ByteString
readRawAt dir file =
    liftEitherWith ReadFailed =<< liftSafeIO @IOException (readFile $ dir </> file)

writeRawAt
    :: (MonadEffectError e m, MonadIO m) => FilePath -> String -> ByteString -> m ()
writeRawAt dir file bs =
    liftEitherWith WriteFailed =<< liftSafeIO @IOException writeFileSafe
  where
    fullPath :: FilePath
    fullPath = dir </> file

    writeFileSafe :: IO ()
    writeFileSafe = bracketOnError
        (openBinaryTempFile dir $ file <.> "tmp")
        (liftA2 (*>) (hClose . snd) (removeFile . fst))
        \(tmp, h) -> do
            hPutStr h bs
            hClose h

            exists <- doesFileExist fullPath

            when exists $ removeFile fullPath
            renameFile tmp fullPath

backupRawAt
    :: (MonadEffectError e m, MonadIO m) => FilePath -> String -> UTCTime -> m ()
backupRawAt dir file now =
    liftEitherAs BackupFailed
        =<< liftSafeIO @IOException (renameFile oldFilePath newFilePath)
  where
    oldFilePath, newFilePath :: FilePath

    oldFilePath = dir </> file
    newFilePath = dir </> printf ".%s_%s" nowStr file <.> "bak"

    nowStr :: String
    nowStr = map (replaceToDot ":-") $ iso8601Show now

    replaceToDot :: String -> Char -> Char
    replaceToDot targets = bool '.' <$> id <*> (`notElem` targets)

getDirectory
    :: (MonadEffectError e m, MonadIO m)
    => XdgDirectory -> String -> EffectError -> m FilePath
getDirectory xdgDir dirName err =
    liftSafeIO @IOException
        (getXdgDirectory xdgDir dirName >! createDirectoryIfMissing True)
        >>= liftEitherAs err

directoryName :: String
directoryName = "todo"

dataFileName :: String
dataFileName = "todo.dat"

configFileName :: String
configFileName = "config.toml"
