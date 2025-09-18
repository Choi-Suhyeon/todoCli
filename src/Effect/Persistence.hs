module Effect.Persistence (readData, writeData) where

import Prelude hiding (readFile)

import System.Directory.Internal (ignoreIOExceptions)
import Control.Exception         (bracketOnError)
import System.Directory  
    ( XdgDirectory(..)         , getXdgDirectory , doesFileExist
    , createDirectoryIfMissing , removeFile      , renameFile
    )
import Data.ByteString           (ByteString, readFile, hPutStr)
import System.FilePath           ((</>), (<.>))
import Control.Monad             (when)
import System.IO                 (openBinaryTempFile, hClose)

import Effect.Internal

readData :: IO ByteString
readData = (</> dataFileName) <$> getDataDirectory >>= readFile

writeData :: ByteString -> IO ()
writeData bs = getDataDirectory >>= \dir ->
    bracketOnError
        (openBinaryTempFile dir $ dataFileName <.> "tmp")
        (\(tmp, h) -> hClose h *> ignoreIOExceptions (removeFile tmp))
        \(tmp, h) -> do
            hPutStr h bs
            hClose h

            let
                fullPath = dir </> dataFileName

            exists <- doesFileExist fullPath

            when exists $ removeFile fullPath
            renameFile tmp fullPath

getDataDirectory :: IO FilePath
getDataDirectory = getXdgDirectory XdgData dataDirectoryName >! createDirectoryIfMissing True

dataDirectoryName :: String
dataDirectoryName = "todo"

dataFileName :: String
dataFileName = "todo.dat"

