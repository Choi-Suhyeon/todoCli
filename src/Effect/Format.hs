module Effect.Format
    ( RenderConfig (..)
    , Column (..)
    , renderTable
    , renderTableWithout
    ) where

import Text.PrettyPrint.Boxes (Box, hsep, left, render, text, top, vsep)

import Data.List.NonEmpty qualified as NE

import Common.Prelude

data Column a b = Column
    { colName :: a
    , hSpace :: Int
    , needsMinWidth :: Bool
    , extractColStr :: b -> String
    }

data RenderConfig a b = RenderConfig
    { cols :: [Column a b]
    , vSpace :: Int
    , cellMinWidth :: Int
    }

renderTable :: (Eq a, Show a) => RenderConfig a b -> [b] -> String
renderTable = renderTableWithout Nothing

renderTableWithout
    :: forall a b f
     . (Eq a, Foldable f, Show a)
    => f a
    -> RenderConfig a b
    -> [b]
    -> String
renderTableWithout exceptions RenderConfig{..} targets = maybe "" renderWithInfo do
    filteredCols <-
        NE.nonEmpty $ filter (\Column{colName} -> colName `notElem` exceptions) cols

    let
        boxCols = NE.map makeColumnWithTitle filteredCols
        (completeBox, _) =
            foldl1 (\(b1, s1) (b2, s2) -> (hsep @[] s1 top [b1, b2], s2))
                $ NE.zipWith (\box col -> (box, col.hSpace)) boxCols filteredCols

    pure completeBox
  where
    renderWithInfo :: Box -> String
    renderWithInfo box =
        render box
            <> show numOfTasks
            <> bool " tasks\n" " task\n" (numOfTasks < 2)

    makeColumnWithTitle :: Column a b -> Box
    makeColumnWithTitle Column{..} =
        vsep @[] vSpace left [colNameToBox colName, dataToBox targets]
      where
        colNameToBox = text . bool id (ensureMinWidth cellMinWidth) needsMinWidth . show
        dataToBox = vsep vSpace left . map (text . extractColStr)

    numOfTasks :: Int
    numOfTasks = length targets

ensureMinWidth :: Int -> String -> String
ensureMinWidth minWidth = liftA2 (<>) id ((`replicate` ' ') . (minWidth -) . length)
