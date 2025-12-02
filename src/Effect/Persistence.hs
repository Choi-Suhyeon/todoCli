module Effect.Persistence (readData, writeData) where

import Control.Exception (Exception, IOException, bracketOnError, try)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString, hPutStr, readFile)
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
import Prelude hiding (readFile)

import Common
import Effect.Error

readData :: (MonadEffectError e m, MonadIO m) => m ByteString
readData =
    getDataDirectory
        >>= (liftSafeIO @IOException . readFile) . (</> dataFileName)
        >>= liftEitherAs ReadFailed

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

    liftEitherAs WriteFailed result

getDataDirectory :: (MonadEffectError e m, MonadIO m) => m FilePath
getDataDirectory =
    liftSafeIO @IOException
        (getXdgDirectory XdgData dataDirectoryName >! createDirectoryIfMissing True)
        >>= liftEitherAs GettingDataDirectoryFailed

liftSafeIO :: (Exception e, MonadIO m) => IO a -> m (Either e a)
liftSafeIO = liftIO . try

dataDirectoryName :: String
dataDirectoryName = "todo"

dataFileName :: String
dataFileName = "todo.dat"
