module Main (main) where

import Options.Applicative (execParser)

import CliParser.Options (Options(..))
import CliParser (opts)

main :: IO ()
main = main' =<< execParser opts

main' :: Options -> IO ()
main' args = do
    print args 

