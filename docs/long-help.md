# todo

A simple command-line task manager

## Overview
This program provides a minimal command-line interface for managing tasks.
It supports adding, listing, editing, marking, and deleting tasks.
Each command may define define its own flags, but several input rules or concepts are shared across commands.

## Concepts and Input Rules
### Task
- A task consists of a required name and the following optional fields: deadline, status, tags, and memo.
- All fields except the name may be omitted.
- Task names do not need to be unique, but non-unique names may make it difficult or impossible to identify a task by name or by a regular expression, especially for commands such as `edit` or `mark`.

### Tags
- Tags must be given as a **space-separated string** (e.g., `work urgent`).
  - The value of `TAGS` is parsed as one argument. If your shell splits words, quote the entire tag list (e.g., `'work urgent'`).
- Only ASCII letters A-Z (case-insensitive) are allowed.
- Tags are stored in uppercase form.
- A maximum of ten tags may be provided.
- Duplicate tags are ignored.
- Tag order is not significant.

### Importance

The `IMP.` field in `add` and `edit` commands accepts an integer from 1 to 9, where higher values indicate greater importance.

Aliases:

- `low` = 2
- `default` = 4
- `important` = 6
- `critical` = 8

If the importance option is not specified, it defaults to `default` (= 4).

The `list` and `delete` commands support filtering by importance range using the `..` separator:
- Format: `<lower>..<upper>`
- Both bounds accept either a number (1-9) or an alias.
- Either or both bounds may be omitted:
  - Omitting the lower bound defaults to the minimum value (1)
  - Omitting the upper bound defaults to the maximum value (9)
- The range is **inclusive** on both ends (closed interval).

Examples:

- `2..6` — tasks with importance from 2 to 6
- `low..8` — equivalent to `2..8`
- `..5` — tasks with importance up to 5
- `6..` — tasks with importance 6 or higher
- `..` — all tasks (no filtering)

### Dates and Time
The `DEADLINE` field accepts either a date or a full ISO-8601 **local** datetime.

- Date only: YYYY-MM-DD 
  - time defaults to 23:59:59
- Date with time: YYYY-MM-DDTHH:MM:SS

Additional rules:

- Timezones or UTC offsets are not allowed.
- All values are interpreted as local time.

### Status
The Statuses that may be designated differ by command.

allowed statuses:

- list: `done`, `undone`, `due`, `overdue`
- mark: `done`, `undone`
- delete: `done`, `overdue`

### Target Selection
Commands such as `edit`, `mark`, and `delete` require selecting one or more target tasks.

- A task may be targeted by providing a regular expression that matches its name.
- For the `delete` command, multiple targets are allowed.
- For `edit` and `mark`, exactly one target must be specified.

### Filters
- The `list` and `delete` commands apply all provided filters using logical AND (no OR is supported).
- Tag filters are also ANDed: a task matches only if it contains all of the specified tags.

### Logging
By default, successful commands produce no output, following the Unix tradition of "silence on success."
However, you can enable the global `--verbose` flag to display additional information about successful operations.
