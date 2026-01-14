# Changelog for `todo`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Added
- Added importance as a task component.
- Added importance range filtering to list and delete subcommands.
- Added `list --reverse` option.
- Added support for user-defined program configuration via `config.toml` (parsed as TOML 0.5).

### Changed
- Added version information to registry serialization. On deserialization failure, the existing data is backed up and a new registry is created.
- Improved list status line to show priority orientation (`prio high at top/bottom`).
- Emitted warning logs regardless of the global verbose flag.

## [2.1.0] - 2026-01-06

### Added
- Added Dhall configuration files (`package.dhall`, `stack.dhall`, `hie.dhall`, `fourmolu.dhall`) to minimize repetition and cognitive overhead in YAML configs.
- Introduced a project-specific Prelude for consistent defaults and reduced explicit imports.
- Added validation of persisted Todo registry data on load.
- Added automatic recovery logic for corrupted or inconsistent registry state.

### Changed
- Replaced Generic-derived instances with manual implementations for Serialize and Hashable
- Removed lens dependency

### Fixed
- Strengthened edge case verification for TaskId

## [2.0.0] - 2025-12-10

### Added
- Added brief descriptions to `long-help.md` and `README.md`.

### Changed
- [BREAKING] Program name changed from `todoCli` to `todo`.
- Removed the `local-modules` section from `fourmolu.yaml`.

## [1.x] - Summary

### Added
- Added global flag `--verbose`.
- Added flags `-m` and `-M` for setting and unsetting a task memo (previously called "description").
- Added support for tasks with no deadline (boundless deadline).

### Changed
- [BREAKING] Reassigned meanings of flags `-d` and `-D`.  
  - Previously: `-d` set a description, `-D` set a deadline.  
  - Now: `-d` sets a deadline, `-D` unsets a deadline.

### Notes
- For detailed changes, refer to the GitHub commit history.
