# The Road Ahead

## Milestone 0: Reducing Dependencies on `lens`

- Refactor modules that currently rely on lens operators and functions.
- Remove the `Common.Optics` module.
- Remove the `microlens` dependency from `package.yaml`.

## Milestone 1: Supporting Relative Deadlines

- Add syntax for specifying relative deadlines in both the `add` and `edit` commands.
- Add aliases for common relative deadline expressions.
- Allow relative deadlines to refer either to the current day or to a specific date (for `edit` only).

## Milestone 2: Adding the `--interactive` Global Flag

- Add a `--interactive` global flag.
- Require user approval when performing any destructive action.
- Enquire about the intended target when multiple targets are selected in the `edit` and `mark` commands.
