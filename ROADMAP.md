# The Road Ahead

## Milestone 0: Supporting Relative Deadlines

- Add syntax for specifying relative deadlines in both the `add` and `edit` commands.
- Add aliases for common relative deadline expressions.
- Allow relative deadlines to refer either to the current day or to a specific date (for `edit` only).

## Milestone 1: Adding the `--interactive` Global Flag

- Add a `--interactive` global flag.
- Require user approval when performing any destructive action.
- Enquire about the intended target when multiple targets are selected in the `edit` and `mark` commands.

## Milestone 2: Adding the Importance Property

- Add an importance property to tasks to represent priority independent of deadlines.
- Support importance values from `0` (default) to `9` (highest).
- Add aliases for common importance levels.
