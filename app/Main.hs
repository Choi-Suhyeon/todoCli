{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Arrow ((>>>))
import Data.HashSet (HashSet)
import Data.List.NonEmpty (nonEmpty)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (TimeZone, getCurrentTimeZone, localTimeToUTC)
import Data.Vector.NonEmpty (NonEmptyVector)
import Formatting (int, sformat, stext, (%))
import System.Exit (exitFailure, exitSuccess)
import System.IO.Error (ioeGetErrorType, isDoesNotExistErrorType)

import Data.HashSet qualified as HS
import Data.Text qualified as T
import Data.Vector.NonEmpty qualified as NEV

import CliParser
import Common
import Domain
import Domain.Core
    ( checkIdInvariant
    , checkStatusIndexInvariant
    , checkTagIndexInvariant
    , rebuildIds
    )
import Effect
import External.Prelude
import View

type BaseM = ExceptT AppError (WriterT Log IO)

runBaseM :: BaseM a -> IO (Either AppError a, Log)
runBaseM = runWriterT . runExceptT

withLoggedError :: BaseM a -> BaseM a
withLoggedError = (`catchError` liftA2 (>>) (logError . into . show) throwError)

newtype App a = App {unApp :: ReaderT Env (StateT TodoRegistry BaseM) a}
    deriving newtype
        ( Applicative
        , Functor
        , Monad
        , MonadError AppError
        , MonadIO
        , MonadReader Env
        , MonadState TodoRegistry
        , MonadWriter Log
        )

runAppM :: Env -> TodoRegistry -> App a -> BaseM (a, TodoRegistry)
runAppM env reg = flip runStateT reg . flip runReaderT env . (.unApp)

type MonadAppError m = MonadError AppError m

data AppError
    = DomainE DomainError
    | EffectE EffectError
    | ParserE ParserError
    | StorageE StorageError
    | ConfigE ConfigError

instance Show AppError where
    show (DomainE x) = "[E:Logic] " <> show x
    show (EffectE x) = "[E:System] " <> show x
    show (ParserE x) = "[E:Parser] " <> show x
    show (StorageE x) = "[E:Storage] " <> show x
    show (ConfigE x) = "[E:Config] " <> show x

instance From DomainError AppError where
    from = DomainE

instance From EffectError AppError where
    from = EffectE

instance From ConfigError AppError where
    from = ConfigE

data ParserError
    = MultipleTargetsError
    | TargetNotFoundError
    | DeletionTargetNotFound
    | NoOptionsProvided

instance Show ParserError where
    show MultipleTargetsError = "The specified target name refers to multiple tasks"
    show TargetNotFoundError = "The specified target name does not refer to any task"
    show DeletionTargetNotFound = "No target matches the deletion criteria"
    show NoOptionsProvided = "At least one option is required"

data StorageError = SerializationE String

instance Show StorageError where
    show (SerializationE x) = x

instance From SerializationError StorageError where
    from (DeserializationFailed e) = SerializationE e

instance From StorageError AppError where
    from = StorageE

showErrWithoutTag :: AppError -> String
showErrWithoutTag (DomainE x) = show x
showErrWithoutTag (EffectE x) = show x
showErrWithoutTag (ParserE x) = show x
showErrWithoutTag (StorageE x) = show x
showErrWithoutTag (ConfigE x) = show x

main :: IO ()
main = do
    (initVals, logs0) <- runBaseM $ withLoggedError do
        config <- loadConfig
        runtime <- initRuntime

        let
            ?config = config

        opts <- liftIO parseOpts

        pure (Env{config, runtime}, opts)

    (env, opts) <-
        either (const $ printLog' False logs0 >> exitFailure) pure initVals

    (res, logs1) <- runBaseM $ withLoggedError do
        reg0 <- loadRegistryWithBackup env
        ((), reg1) <- runAppM env reg0 $ main' opts

        UsingCereal reg1 & serialize & writeData

    printLog' opts.globalFlags.verbose $ logs0 <> logs1
    bool exitFailure exitSuccess $ isRight res
  where
    printLog' :: Bool -> Log -> IO ()
    printLog' verbose
        | verbose = printLog (/= DiagError)
        | otherwise = printLogExcept (/= DiagError) (Just DiagInfo)

    initRuntime :: (MonadIO m) => m Runtime
    initRuntime = do
        now <- liftIO getCurrentTime
        tz <- liftIO getCurrentTimeZone

        pure Runtime{now, tz}

    loadConfig :: (MonadIO m, MonadLog m) => m Config
    loadConfig = either onFail pure =<< runExceptT load
      where
        load :: (MonadAppError m, MonadIO m) => m Config
        load = liftEitherInto . decodeConfig =<< readConfig

        onFail :: forall m. (MonadIO m, MonadLog m) => AppError -> m Config
        onFail err =
            pure err
                >! createDefaultIfMissing
                >>= (logWarning . failedToLoad)
                >> pure initConfig
          where
            createDefaultIfMissing :: AppError -> m ()
            createDefaultIfMissing (EffectE (ReadFailed e)) =
                when
                    (isDoesNotExistErrorType $ ioeGetErrorType e)
                    ( (>>=)
                        (runExceptT $ writeConfig @AppError $ encodeConfig initConfig)
                        (either (logWarning . failedToWriteConfig) (const $ pure ()))
                    )
            createDefaultIfMissing _ = pure ()

            failedToLoad :: AppError -> Text
            failedToLoad = warningMsg "Config loading failed: "

            failedToWriteConfig :: AppError -> Text
            failedToWriteConfig = warningMsg "Config file creation failed: "

            warningMsg :: Text -> AppError -> Text
            warningMsg d e = d <> into (showErrWithoutTag e)

    loadRegistryWithBackup
        :: (MonadAppError m, MonadIO m, MonadLog m) => Env -> m TodoRegistry
    loadRegistryWithBackup Env{runtime = Runtime{now}} =
        loadRegistry >>= \case
            Left e@(StorageE (SerializationE _)) -> do
                let
                    msg1, msg2 :: Text

                    msg1 = "Registry deserialization failed: " <> into (showErrWithoutTag e)
                    msg2 =
                        "Attempting to back up the existing registry; if successful, a new registry will be created."

                logWarning msg1
                logWarning msg2
                initTodoRegistry <$ backupData now
            other -> liftEither other

    loadRegistry :: (MonadIO m) => m (Either AppError TodoRegistry)
    loadRegistry = do
        result <- runExceptT do
            raw <- readData
            UsingCereal reg <-
                raw
                    & deserialize @(UsingCereal TodoRegistry)
                    & first (into @StorageError)
                    & liftEitherInto @AppError

            liftEitherInto $ adjust reg

        pure case result of
            reg@(Right _) -> reg
            err@(Left (EffectE (ReadFailed e)))
                | isDoesNotExistErrorType $ ioeGetErrorType e -> Right initTodoRegistry
                | otherwise -> err
            err@(Left _) -> err
      where
        adjust :: TodoRegistry -> Either DomainError TodoRegistry
        adjust = liftA3 bool rebuildIds pure (and . (<$> invariants) . (&))

        invariants :: [TodoRegistry -> Bool]
        invariants = [checkIdInvariant, checkTagIndexInvariant, checkStatusIndexInvariant]

type UsingGlobalFlag = ?gFlags :: GlobalFlags

main' :: Options -> App ()
main' Options{optCommand, globalFlags} = runCommand optCommand
  where
    ?gFlags = globalFlags

runCommand :: (UsingGlobalFlag) => Command -> App ()
runCommand (Add x) = runAddCommand x
runCommand (List x) = runListCommand x
runCommand (Edit x) = runEditCommand x
runCommand (Mark x) = runMarkCommand x
runCommand (Delete x) = runDeleteCommand x

runAddCommand :: AddCommand -> App ()
runAddCommand AddCommand{name, deadline, memo, tags, importance} =
    optionDeadlineToEntryDeadline deadline
        >>= \utcDeadline -> addTask EntryCreation{name, memo, tags, importance, deadline = utcDeadline}

runListCommand :: ListCommand -> App ()
runListCommand ListCommand{tags, status, importance, columns, shouldReverse} = do
    tz <- asks (.runtime.tz)
    allTasks <- getAllTasks
    tasks1 <- traverse getTasksWithAllTags tags
    tasks2 <- traverse getTasksWithinImportanceRange importance
    tasks3 <- (`traverse` status) \case
        LstDone -> getDoneTasks
        LstUndone -> getUndoneTasks
        LstDue -> getDueTasks
        LstOverdue -> getOverdueTasks

    snapshots <-
        [tasks1, tasks2, tasks3]
            & catMaybes
            & foldl' HS.intersection allTasks
            & getTaskDetails
            & fmap (sortTaskDetails >>> bool id reverse shouldReverse)

    let
        numOfSnapshots :: Int
        numOfSnapshots = length snapshots

        infoToShow :: Text
        infoToShow =
            sformat
                (int % " task" % stext % " (prio high at " % stext % ")\n")
                numOfSnapshots
                (bool "" "s" (numOfSnapshots > 1))
                (bool "bottom" "top" shouldReverse)

        renderConfig :: TaskDetailRenderConfig
        renderConfig = renderConfig' columns tz infoToShow
          where
            renderConfig' =
                maybe
                    initTaskDetailRenderConfig
                    (initTaskDetailRenderConfigWith . flip elem . map toViewColName)

    logOutput $ renderTable renderConfig snapshots

runEditCommand :: (UsingGlobalFlag) => EditCommand -> App ()
runEditCommand EditCommand{tgtName, name, deadline, memo, tags, importance} =
    traverse optionDeadlineToEntryDeadline deadline >>= \utcDeadline ->
        getTasksByNameRegex tgtName
            <&> toList
            >>= resolveTaskId
            >>= editIfexists utcDeadline
  where
    editIfexists :: Maybe EntryDeadline -> Maybe TaskId -> App ()
    editIfexists utcDeadline =
        traverse_
            $ editTask
                EntryPatch
                    { name
                    , importance
                    , status = Nothing
                    , deadline = utcDeadline
                    , memo = processMemo memo
                    , tags = processTags tags
                    }

    processMemo :: Maybe EditMemo -> Maybe Text
    processMemo (Just Remove) = Just ""
    processMemo (Just (Memo m)) = Just m
    processMemo Nothing = Nothing

    processTags :: Maybe EditTags -> Maybe (HashSet Text)
    processTags (Just Clear) = Just mempty
    processTags (Just (Substitute s)) = Just s
    processTags Nothing = Nothing

runMarkCommand :: (UsingGlobalFlag) => MarkCommand -> App ()
runMarkCommand mrk =
    getTasksByNameRegex tgtName
        <&> toList
        >>= resolveTaskId
        >>= traverse_ (markTask status)
  where
    status :: EntryStatus
    tgtName :: Text

    (status, tgtName) = case mrk of
        MrkDone t -> (EDone, t)
        MrkUndone t -> (EUndone, t)

runDeleteCommand :: (UsingGlobalFlag) => DeleteCommand -> App ()
runDeleteCommand del = do
    tz <- asks (.runtime.tz)
    tgtIds <-
        gatherTargets del >>= \case
            Nothing -> throwError $ ParserE NoOptionsProvided
            Just [] -> throwError $ ParserE DeletionTargetNotFound
            Just xs -> pure xs

    (`when` deleteTasks tgtIds)
        =<< bool (pure True) (doInteraction tz tgtIds) ?gFlags.interactive
  where
    gatherTargets :: DeleteCommand -> App (Maybe (HashSet TaskId))
    gatherTargets DelAll = pure <$> getAllTasks
    gatherTargets DelBy{byName, byTags, byStatus, byImportance} = do
        tasks1 <- traverse getTasksByNameRegex byName
        tasks2 <- traverse getTasksWithAllTags byTags
        tasks3 <- traverse getTasksWithinImportanceRange byImportance
        tasks4 <- (`traverse` byStatus) \case
            DelDone -> getDoneTasks
            DelOverdue -> getOverdueTasks

        [tasks1, tasks2, tasks3, tasks4]
            & catMaybes
            & nonEmpty
            & fmap (foldr1 HS.intersection)
            & pure

    doInteraction :: (Foldable f) => TimeZone -> f TaskId -> App Bool
    doInteraction tz =
        chooseYesOrNoUntil
            . decorateYesNoMsg
            . renderTasks
            <=< pure
            . catMaybes
            <=< traverse getOneTaskDetail
            . toList
      where
        renderTasks :: (Foldable f) => f TaskDetail -> Text
        renderTasks = foldr ((<>) . renderTask) T.empty

        renderTask :: TaskDetail -> Text
        renderTask = ("  " <>) . (<> "\n") . renderTaskConcise tz

resolveTaskId
    :: (Traversable t, UsingGlobalFlag) => t TaskId -> App (Maybe TaskId)
resolveTaskId targetIds = do
    tz <- asks (.runtime.tz)
    targetDetails <- traverse getOneTaskDetail targetIds
    targets <-
        zip (toList targetIds) (toList targetDetails)
            & mapMaybe (\(i, md) -> (i,) <$> md)
            & toNonEmptyVector
            & maybe (throwError $ ParserE TargetNotFoundError) pure
    bool
        (Just . fst <$> fromSingleton targets)
        (doInteraction tz targets)
        ?gFlags.interactive
  where
    toNonEmptyVector :: (Foldable f) => f a -> Maybe (NonEmptyVector a)
    toNonEmptyVector = NEV.fromList . toList

    fromSingleton :: (Foldable f, MonadAppError m) => f a -> m a
    fromSingleton = (. toList) \case
        [x] -> pure x
        [] -> throwError $ ParserE TargetNotFoundError
        _ -> throwError $ ParserE MultipleTargetsError

    doInteraction
        :: TimeZone -> NonEmptyVector (TaskId, TaskDetail) -> App (Maybe TaskId)
    doInteraction tz =
        liftA3 bool (confirmOne . NEV.head) selectOne ((> 1) . length)
      where
        selectOne :: NonEmptyVector (TaskId, TaskDetail) -> App (Maybe TaskId)
        selectOne = fmap (Just . fst) . chooseAmongUntil (renderTaskConcise tz . snd)

        confirmOne :: (TaskId, TaskDetail) -> App (Maybe TaskId)
        confirmOne (tid, detail) = do
            choice <-
                renderTaskDetail tz detail
                    & decorateYesNoMsg
                    & chooseYesOrNoUntil

            pure $ bool Nothing (Just tid) choice

optionDeadlineToEntryDeadline
    :: (MonadEnv m) => OptionDeadline -> m EntryDeadline
optionDeadlineToEntryDeadline Boundless = pure EBoundless
optionDeadlineToEntryDeadline (Bound d) = asks (.runtime.tz) >>= pure . EBound . (`localTimeToUTC` d)

toViewColName :: ListColumns -> ViewColName
toViewColName LstName = CNName
toViewColName LstMemo = CNMemo
toViewColName LstTags = CNTags
toViewColName LstStatus = CNStatus
toViewColName LstDeadline = CNDeadline
toViewColName LstImportance = CNImportance

decorateYesNoMsg :: Text -> Text
decorateYesNoMsg =
    ("Target:\n" <>)
        . (<> "[?] This action is destructive.\n[?] Do you want to continue")
