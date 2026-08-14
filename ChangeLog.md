# Changelog

## Unreleased

- Renamed package from `mealy` to `process-stats`.
- Replaced the hand-rolled `Data.Mealy` arrow with `Circuit.Process.Process`
  from `circuits`.
- Modules renamed: `Data.Mealy` → `Process.Stats`, `Data.Mealy.Diff` →
  `Process.Stats.Diff`, `Data.Mealy.Trace` → `Process.Stats.Simulate`.
- Re-export `Circuit.Process.fold`; the local throwing `fold` is gone.
- Move `MultiplicativeAction Double` / `DivisiveAction Double` orphan instances
  upstream to `numhask`.
- Add `circuits-ad` dependency and `process-stats-axioma` oracle suite (P1–P10).
- Verify `Process.Stats.Diff` gradients against hand-derived references and
  against `circuits-ad`.

## 0.5.2.0

- Added `Data.Mealy.Diff` and `Data.Mealy.Trace` modules.
- Added `Additive` and `Subtractive` instances for `Averager`.
- Added local `circuits` and `numhask-free` dependencies.
- Migrated internals to `Circuit.Trace` / `Circuit.Monoidal` API.
