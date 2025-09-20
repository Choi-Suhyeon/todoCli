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
import Data.Function             (on)
import System.IO                 (openBinaryTempFile, hClose)


import Util

readData :: IO ByteString
readData = (</> dataFileName) <$> getDataDirectory >>= readFile

writeData :: ByteString -> IO ()
writeData bs = getDataDirectory >>= \dir ->
    bracketOnError
        (openBinaryTempFile dir $ dataFileName <.> "tmp")
        ((liftA2 (*>) `on` (ignoreIOExceptions .)) (hClose . snd) (removeFile . fst))
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

