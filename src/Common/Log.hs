module Common.Log
    ( MonadLog
    , Log
    , DiagLevel (..)
    , logOutput
    , logInfo
    , logWarning
    , logError
    , printLog
    , printLogExcept
    ) where

import Data.Sequence (Seq)
import Data.Text (Text)
import System.IO (stderr, stdout)

import Data.Sequence qualified as SQ
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

import External.Prelude

type MonadLog m = MonadWriter Log m

data Log = Log
    { out :: Seq Text
    , diag :: Seq Diagnostic
    }

instance Semigroup Log where
    l1 <> l2 =
        Log
            { out = l1.out <> l2.out
            , diag = l1.diag <> l2.diag
            }

instance Monoid Log where
    mempty = Log{out = mempty, diag = mempty}

data Diagnostic = Diagnostic
    { level :: DiagLevel
    , message :: Text
    }

data DiagLevel
    = DiagInfo
    | DiagWarning
    | DiagError
    deriving (Eq)

renderDiagLevelTag :: DiagLevel -> Text
renderDiagLevelTag DiagInfo = "[I]"
renderDiagLevelTag DiagWarning = "[W]"
renderDiagLevelTag DiagError = "[E]"

logOutput, logInfo, logWarning, logError :: (MonadLog m) => Text -> m ()
logOutput m = tell Log{diag = mempty, out = [m]}
logInfo = logDiagnostic DiagInfo
logWarning = logDiagnostic DiagWarning
logError = logDiagnostic DiagError

printLog :: (DiagLevel -> Bool) -> Log -> IO ()
printLog prefixNeeded = printLogExcept prefixNeeded Nothing

printLogExcept
    :: (Foldable f) => (DiagLevel -> Bool) -> f DiagLevel -> Log -> IO ()
printLogExcept prefixNeeded exclusion Log{out, diag} = do
    printDiag diag
    traverse_ (TIO.hPutStrLn stdout) $ out
  where
    printDiag :: Seq Diagnostic -> IO ()
    printDiag =
        liftA2 unless T.null (TIO.hPutStr stderr . (<> "\n\n"))
            . foldMap id
            . SQ.intersperse "\n"
            . fmap toText
            . SQ.filter ((`notElem` exclusion) . (.level))

    toText :: Diagnostic -> Text
    toText = toTextOfDiagnostic <$> prefixNeeded . (.level) <*> id

logDiagnostic :: (MonadLog m) => DiagLevel -> Text -> m ()
logDiagnostic level message = tell Log{out = mempty, diag = [Diagnostic{level, message}]}

toTextOfDiagnostic :: Bool -> Diagnostic -> Text
toTextOfDiagnostic prefixEnabled Diagnostic{level, message} =
    (<> message)
        if prefixEnabled
            then renderDiagLevelTag level <> " "
            else ""
