module Effect.Persistence (readData, writeData) where

import Prelude hiding (readFile)

import Control.Monad.IO.Class    (MonadIO(..))
import Control.Exception         (Exception, IOException, bracketOnError, try)
import System.Directory  
    ( XdgDirectory(..)         , getXdgDirectory , doesFileExist
    , createDirectoryIfMissing , removeFile      , renameFile
    )
import Data.ByteString           (ByteString, readFile, hPutStr)
import System.FilePath           ((</>), (<.>))
import Control.Monad             (when)
import System.IO                 (openBinaryTempFile, hClose)

import Effect.Error
import Common

readData :: (MonadIO m, MonadEffectError e m) => m ByteString
readData 
    =   (</> dataFileName) <$> getDataDirectory 
    >>= liftSafeIO @IOException . readFile 
    >>= liftEitherAs ReadFailed

writeData :: (MonadIO m, MonadEffectError e m) => ByteString -> m ()
writeData bs = do
    dir    <- getDataDirectory
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
        
getDataDirectory :: (MonadIO m, MonadEffectError e m) => m FilePath
getDataDirectory 
    =   liftSafeIO @IOException (getXdgDirectory XdgData dataDirectoryName >! createDirectoryIfMissing True) 
    >>= liftEitherAs GettingDataDirectoryFailed

liftSafeIO :: (Exception e, MonadIO m) => IO a -> m (Either e a)
liftSafeIO = liftIO . try

dataDirectoryName :: String
dataDirectoryName = "todo"

dataFileName :: String
dataFileName = "todo.dat"
