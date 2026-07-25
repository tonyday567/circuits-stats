# Changelog

## Unreleased

- Renamed package from `mealy` to `process-stats`.
- Replaced the hand-rolled `Data.Mealy` arrow with `Circuit.Process.Process`
  from `circuits`.
- Modules renamed: `Data.Mealy` → `Process.Stats`, `Data.Mealy.Diff` →
  `Process.Stats.Diff`, `Data.Mealy.Trace` → `Process.Stats.Simulate`.

## 0.5.2.0

- Added `Data.Mealy.Diff` and `Data.Mealy.Trace` modules.
- Added `Additive` and `Subtractive` instances for `Averager`.
- Added local `circuits` and `numhask-free` dependencies.
- Migrated internals to `Circuit.Trace` / `Circuit.Monoidal` API.
