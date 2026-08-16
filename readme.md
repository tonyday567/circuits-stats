# circuits-stats

A `Process` is a triple of functions:

- `(a -> s)` **inject**: convert an input into the initial state.
- `(s -> a -> s)` **step**: update state given prior state and new input.
- `(s -> b)` **extract**: convert state to output.

A sum, for example, looks like `M id (+) id` where the first `id` is the
initial injection and the second `id` is the covariant extraction.

This library provides support for computing statistics (such as an average or
a standard deviation) as current state within a process context. The carrier
type now lives in `circuits` as `Circuit.Process.Process`; this package is the
statistical interpretation built on top of it.

## Circuits ecosystem relationship

`circuits-stats` is a client of the `circuits` ecosystem:

- The state-machine arrow is `Circuit.Process.Process` from `circuits`.
- Reverse-mode gradients share the same `NumHask.Diff.Diff` primitive arrow
  that `circuits-ad` builds on; `Circuit.Stats.Diff` is verified against
  `circuits-ad` in the `circuits-stats-axioma` oracle suite (P10).
- Scalar self-actions for `Double` (used by the ODE integrators) now live in
  `numhask`, removing the orphan instances that previously lived in
  `Circuit.Stats.ODE`.

The statistical implementations (`online`, `ma`, `sqma`, `std`, `cov`,
`reg`, quantiles, etc.) remain the canonical `circuits-stats` reference
implementations.

## Naming note

In the strict automata-theory sense, the type here is a **Moore machine**: the
output depends only on the current state, not on the current input. A
categorical machine with input-dependent output would have type
(`s -> a -> b`). The library keeps the streaming, input-driven transition
`step :: s -> a -> s`, but the carrier is now the polymorphic `Process` arrow
from `circuits`.

## Usage

Supply a decay function representing the relative weights of recent values
versus older ones, in the manner of exponentially-weighted averages. The
library attempts to be polymorphic in the statistic, which can be combined in
applicative style.

```haskell
import Prelude
import Data.Maybe (fromMaybe)
import Circuit.Stats
```

```haskell
fromMaybe (0/0) $ fold ((,) <$> ma 0.9 <*> std 0.9) [1..100::Double]
```

```
(91.00265621044142,9.472822289121)
```

`fold` is re-exported from `Circuit.Process.fold` and is therefore total:
it returns `Nothing` for an empty input list and `Just` the final output
otherwise.

## Backport notes

- `fold` is now the total `Circuit.Process.fold`.
- `Circuit.Stats.ODE` no longer contains orphan `MultiplicativeAction Double`
  / `DivisiveAction Double` instances; they have moved upstream to
  `NumHask.Algebra.Action`.
- `Circuit.Stats.Diff` keeps its stable API. The high-level runners
  (`gradScan`, `gradFold`) already use `NumHask.Diff` directly; the
  lower-level `DiffProcess` / `DiffSystem` capture-and-replay machinery is
  retained because it does not have a direct, API-preserving translation to
  `circuits-ad`'s `Net`-based `linearizeAt` / `backprop`. The oracle suite
  guards the current behaviour and cross-checks it against `circuits-ad`.

## Reference

[Finite State Transducers](https://stackoverflow.com/questions/27997155/finite-state-transducers-in-haskell)
