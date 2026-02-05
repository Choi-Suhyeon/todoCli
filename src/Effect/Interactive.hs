module Effect.Interactive
    ( chooseYesOrNoOnce
    , chooseYesOrNoUntil
    , chooseAmongOnce
    , chooseAmongUntil
    ) where

import Data.NumberLength (numberLength)
import Data.Text (Text)
import Data.Vector.NonEmpty (NonEmptyVector)
import Formatting (left, sformat, stext, (%), (%+))
import System.IO (hFlush, stderr)
import Text.Read (readMaybe)

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector.NonEmpty qualified as NEV

import Common
import Effect.Error
import External.Prelude

chooseYesOrNoOnce :: (MonadEffectError e m, MonadIO m) => Text -> m Bool
chooseYesOrNoOnce = inputOnce parseYesNo . addYesNoPrompt

chooseYesOrNoUntil :: (MonadIO m) => Text -> m Bool
chooseYesOrNoUntil = inputUntil (eitherToMaybe . parseYesNo) . addYesNoPrompt

chooseAmongOnce
    :: forall a m e
     . (MonadEffectError e m, MonadIO m) => (a -> Text) -> NonEmptyVector a -> m a
chooseAmongOnce f xs =
    printOptions f xs
        >> inputOnce (parseChoice xs) "[?] choose the number: "

chooseAmongUntil
    :: forall a m. (MonadIO m) => (a -> Text) -> NonEmptyVector a -> m a
chooseAmongUntil f xs =
    printOptions f xs
        >> inputUntil (eitherToMaybe . parseChoice xs) "[?] choose the number: "

parseYesNo :: Text -> Either EffectError Bool
parseYesNo t
    | elem @[] upperStripped ["Y", "YES"] = Right True
    | elem @[] upperStripped ["N", "NO"] = Right False
    | otherwise = Left $ InvalidUserInput $ into stripped
  where
    stripped = T.strip t
    upperStripped = T.toUpper stripped

parseChoice :: NonEmptyVector a -> Text -> Either EffectError a
parseChoice xs t =
    maybeToEither
        (InvalidUserInput $ strippedStr)
        (readMaybe strippedStr >>= (xs NEV.!?))
  where
    strippedStr = into $ T.strip t

addYesNoPrompt :: Text -> Text
addYesNoPrompt = (<> " [y/n]? ")

printOptions
    :: forall a m. (MonadIO m) => (a -> Text) -> NonEmptyVector a -> m ()
printOptions f xs =
    traverse_
        (\(i, x) -> liftIO (TIO.hPutStrLn stderr $ mkOptText i x))
        (NEV.imap (,) xs)
  where
    spaceSize :: Int
    spaceSize = length xs & pred & numberLength

    mkOptText :: Int -> a -> Text
    mkOptText idx x = sformat ("[" % left spaceSize ' ' % "]" %+ stext) idx (f x)

inputOnce
    :: (MonadEffectError e m, MonadIO m)
    => (Text -> Either EffectError a) -> Text -> m a
inputOnce f ques = either throwErrorInto pure . f =<< input ques

inputUntil :: (MonadIO m) => (Text -> Maybe a) -> Text -> m a
inputUntil f ques = maybe (inputUntil f ques) pure . f =<< input ques

input :: (MonadIO m) => Text -> m Text
input ques = liftIO $ TIO.hPutStr stderr ques >> hFlush stderr >> TIO.getLine
