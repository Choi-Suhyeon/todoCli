# Changelog for `todo`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Added
- Added Dhall configuration files (`package.dhall`, `stack.dhall`, `hie.dhall`, `fourmolu.dhall`) to minimize repetition and cognitive overhead in YAML configs.

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
