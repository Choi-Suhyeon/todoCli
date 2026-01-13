# Architecture - Module Structure and Visibility Policy

## High-level module separation
- `CliParser`, `Domain`, and `Effect` are mutually independent and **must not depend on each other**.
- Each of these modules defines its own error type, conforming to `Common.AbstractError`.

### Responsibilities
- **Domain**
  - Encapsulates business logic.
  - `Domain.Core` is strictly pure and must remain free from *any* kind of side effects.
- **Effect**
  - Handles IO boundaries such as output formatting, persistence, and external interactions.
- **CliParser**
  - Responsible for command-line argument parsing.
  - Enforces syntactic invariants of CLI inputs.

## Common module
- `Common` may be imported by all modules.
- `Common` **must not import any user-defined modules**, except for its own submodules.

## View module
- `View` may depend on Domain types intended for presentation and on formatting utilities from `Effect.Format`, but should not depend on persistence or other effectful boundaries.
- It is responsible for adapting domain-level data and logic to concrete output formats.

## Import rules
- Except for `Common`, modules should generally import only:
  - Their own submodules
  - Modules at the same hierarchical level
  
### Façade convention
- When a module file and a directory with the same name coexist at the same level (e.g. `X.hs` and `X/`), the file module **must**, at minimum, act as a façade (public API and/or re-exports).
- Consumers are expected to import `X` rather than its submodules directly.

## Type-oriented module layout
- Each core type should be defined in its own module.
- Types are encouraged to have `Raw` and `Internal` submodules.

### Submodule conventions
- `Type.Raw`
  - Exposes the underlying representation directly.
  - Importing `Type.Raw` is allowed only in invariant/security modules (and tests). Any other use requires justification.
- `Type.Internal`
  - Provides low-level or extended APIs.
  - May be imported by modules at the same level as `Type`.
- `Type`
  - Defines the public API exposed across module boundaries.
  - Public-facing modules re-export these APIs at the boundary.
- For values that must remain hidden, **DTOs are introduced instead of exposing internals directly**.
