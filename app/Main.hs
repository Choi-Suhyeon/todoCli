module Main (main) where

import Control.Monad.State.Strict (MonadState, StateT, runStateT)
import Control.Monad.IO.Class     (MonadIO)
import Control.Monad.Reader       (MonadReader, ReaderT, runReaderT)
import Data.Time.LocalTime        (getCurrentTimeZone)
import Control.Exception          (IOException, try, catch)
import Data.Time.Clock            (getCurrentTime)
import Data.Bifunctor             (first)
import Data.Function              ((&))
import GHC.Generics               (Generic)
import System.Exit                (die)
import Lens.Micro                 ((^.))

import Domain.Serialization
import CliParser 
import Domain
import Effect
import Env

newtype App a = App { unApp :: ReaderT Env (StateT TodoRegistry IO) a }
  deriving stock
    ( Generic
    )
  deriving newtype 
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Env
    , MonadState TodoRegistry
    )

runApp :: Env -> TodoRegistry -> App a -> IO (a, TodoRegistry)
runApp env reg (App m) = runStateT (runReaderT m env) reg

data AppErr 
    = IOE     IOException 
    | DomainE ErrCode
  deriving (Generic)

main :: IO ()
main = do
    opts       <- parseOpts
    env        <- initEnv
    reg        <- loadRegistry
    ((), reg') <- runApp env reg $ main' opts

    catch @IOException
        (UsingCereal reg' & serialize & writeData)
        (const $ die "[E] Failed to save data")
  where
    initEnv :: IO Env
    initEnv = do
        now <- getCurrentTime
        tz  <- getCurrentTimeZone

        pure Env{ now, tz }

    loadRegistry :: IO TodoRegistry
    loadRegistry 
        =   first IOE <$> try readData 
        >>= 
            pure 
            . either (const initTodoRegistry) (^. #unUsingCereal) 
            . (>>= first DomainE . deserialize @(UsingCereal TodoRegistry))


main' :: Options -> App ()
main' Options { optCommand } = runCommand optCommand 

runCommand :: Command -> App ()
runCommand (Add    x) = runAddCommand    x
runCommand (List   x) = runListCommand   x
runCommand (Edit   x) = runEditCommand   x
runCommand (Mark   x) = runMarkCommand   x 
runCommand (Delete x) = runDeleteCommand x

runAddCommand :: AddCommand -> App ()
runAddCommand AddCommand{ name, deadline, desc, tags } = do
    r <- addTask EntryCreate{ name, desc, tags, deadline }

runListCommand :: ListCommand -> App ()
runListCommand ListCommand{ tags, status } = undefined

runEditCommand :: EditCommand -> App ()
runEditCommand EditCommand{ tgtName, name, deadline, desc, tags } = undefined 

runMarkCommand :: MarkCommand -> App ()
runMarkCommand (MrkDone tgtName) = undefined 
runMarkCommand (MrkUndone tgtName) = undefined 

runDeleteCommand :: DeleteCommand -> App ()
runDeleteCommand DelAll = undefined 
runDeleteCommand DelBy{ byName, byTags, byStatus } = undefined 
