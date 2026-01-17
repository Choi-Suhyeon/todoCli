# Architecture - Module Structure and Visibility Policy

## Major module separation
- `CliParser`, `Domain`, and `Effect` are mutually independent and **must not depend on each other**.
- Each of these modules defines its own error type, conforming to `Common.AbstractError`.

### Responsibilities
- **Domain**
  - Encapsulates business logic.
  - `Domain.Core` is strictly pure and must remain free from *any* side effects.
- **Effect**
  - Handles IO boundaries such as persistence and external interactions.
- **CliParser**
  - Responsible for command-line argument parsing.
  - Enforces syntactic invariants of CLI inputs.
  
## External module
- `External` may be imported by all modules.
- It contains wrapper modules for external libraries.
- It does not provide a facade module.

## Common module
- `Common` may be imported by all modules.
- It contains both utilities and abstract structures usable across modules regardless of their specific purpose.

## View module
- `View` may depend on Domain types intended for presentation.
- It is responsible for adapting domain-level data and logic to concrete output formats.

## Import rules
- Modules should generally import only:
  - Their own submodules
  - Modules at the same hierarchical level
  
### Facade convention
- When a module file and a directory with the same name coexist at the same level (e.g., `X.hs` and `X/`), the module file **must** act as a facade (public API and/or re-exports).
- Consumers are expected to import `X` rather than its submodules directly.

## Type-oriented module layout
- Each core type should be defined in its own module.
- Types are encouraged to have `Raw` and `Internal` submodules.

### Submodule conventions
- `Type.Raw`
  - This module exposes the underlying representation directly.
  - It may only be imported in invariant/security modules (and tests). Any other use requires explicit justification.
- `Type.Internal`
  - This module provides low-level or extended APIs.
  - It may be imported by modules at the same hierarchical level as `Type`.
- `Type`
  - This module defines the public API exposed across module boundaries.
  - Public-facing modules re-export these APIs at boundaries.
- For values that must remain hidden, **DTOs are introduced instead of exposing internals directly**.
