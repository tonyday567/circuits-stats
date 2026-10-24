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

## Decay ⟜ what the decay rate is

Every estimator in the library is `online f g` (`Circuit.Stats`):

    sₙ = r·sₙ₋₁ + f(xₙ)     cₙ = r·cₙ₋₁ + 1     estimate = sₙ / cₙ

The decayed count cₙ = (1 − rⁿ)/(1 − r) normalizes the decayed sum, so an
observation k steps back carries normalized weight (1 − r)rᵏ. Three
constants of the same object:

- half-life ⟜ h = −ln 2 / ln r, the age at which an observation's weight
  halves; h ≈ ln2 / (1 − r) for r near 1.
- decayed count ⟜ cₙ → 1 / (1 − r), the sum of weights; r = 1 recovers
  the plain average (memory grows as n), r = 0 the latest value (memory
  1). These corners are pinned as doctests on `ma`.
- effective sample size ⟜ ESS = (1 + r) / (1 − r) (participation ratio
  of the weight profile). For iid data with variance σ², the variance of
  the estimate is ≈ σ² / ESS, so r is the bias/variance dial: it trades
  responsiveness to the recent regime against estimator variance.

The same recursion, identified in the time-series literature:

- `sqma r` over residuals is an EWMA variance estimator — the
  RiskMetrics (1994) model — equivalently a restricted GARCH(1,1) with
  ω = 0 and α + β = 1, i.e. the IGARCH boundary: variance shocks persist
  indefinitely, decaying at rate r. The persistence is total by
  construction; the library never estimates ω, α, β separately.
- `beta` / `reg` step the regression sufficient statistics (xx, x, xy,
  y) with the same recursion: discounted least squares, which is the
  steady-state constant-gain Kalman filter for a slowly-drifting
  parameter — the gain is pinned by r rather than adapted.
- hft's `stdDecaySensitivity` walks the r-curve empirically (R² of
  |return| ~ std for r ∈ [0.001, 0.05]); fast trading decays (r = 0.01,
  memory ≈ 1) sit beside slow quantile decays (r = 0.99, memory ≈ 100).

Finally, `Circuit.Stats.Diff` differentiates the whole scan, decay rate
included: r itself is a learnable parameter, fittable by gradient
descent against any downstream loss. That makes the two-timescale shape
explicit — fast statistics inside the scan, a slowly-learned decay
outside it.

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
