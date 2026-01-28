module Domain.Core.Task
    ( -- * Public Types
      Task

      -- * Public InBound Types
    , EntryCreation (..)
    , EntryPatch (..)
    , EntryDeadline (..)
    , EntryStatus (..)

      -- * Public Outbound Types
    , TaskBasic (..)
    , TaskBasicStatus (..)
    , TaskBasicDeadline (..)

      -- * Public Value
    , nameLenBound
    , memoLenBound

      -- * Public(Domain) API
    , mkTask
    , modifyTask
    , toTaskBasic
    ) where

import Domain.Core.Task.Internal
