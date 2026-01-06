module Domain.Core.TodoRegistry
    ( TodoRegistry

      -- * Public Construction
    , initTodoRegistry

      -- * Internal(Domain) Query
    , isIdInUse
    , getTaskById
    , getDoneTasks
    , getUndoneTasks
    , getTasksUndoneAnd
    , getTasksWithAllTags
    , getTasksMatching

      -- * Internal(Domain) Update/Delete
    , insertTask
    , replaceTask
    , deleteTask
    )
where

import Domain.Core.TodoRegistry.Internal
