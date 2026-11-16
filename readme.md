# circuits-stats

`circuits-stats` provides streaming statistics as Moore machines. The
constructors return `Circuit.Machine.MooreT` values from `circuits`, with the
machine's state type hidden behind the `MooreT` wrapper.

`Circuit.Machine.moore` builds a machine from three functions:

- `(a -> s)` **init**: convert the first input into the initial state.
- `(s -> a -> s)` **step**: update state given prior state and new input.
- `(s -> b)` **extract**: convert state to output.

A sum, for example, is `moore id (+) id`: the first input initializes the
sum, subsequent inputs extend it, and the current sum is the output.

## Circuits ecosystem relationship

`circuits-stats` is a client of two packages:

- `Circuit.Machine` from `circuits` supplies the state-machine representation
  and its runners.
- `Circuit.Diff` from `circuits-diff` supplies the `Diff` carrier used by
  `Circuit.Stats.Diff` for reverse-mode gradients through scans and folds.

The scalar actions used by `Circuit.Stats.ODE` come from
`NumHask.Algebra.Action` in `numhask`.

The statistical implementations (`online`, `ma`, `sqma`, `std`, `cov`,
`reg`, quantiles, etc.) are the `circuits-stats` reference implementations.

## Moore machines

The output of a Moore machine depends on its current state. Each input first
updates the state through `step :: s -> a -> s`; `extract :: s -> b` then
reads the output. The statistical constructors use
`MooreT (,) (->) a b`, so machines with different hidden state types can be
combined applicatively and run over the same input.

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

`Circuit.Stats` re-exports `MooreT`, `moore`, `before`, `scan`, and `fold`
from `Circuit.Machine` as a convenience surface for statistical machines.
`scan` returns one output per input. `fold` returns `Nothing` for an empty
input list and `Just` the final output otherwise.

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

## Differentiation and verification

`Circuit.Stats.Diff` runs Moore machines with the `Circuit.Diff.Diff` carrier:

- `gradScan` and `gradFold` return pullbacks with respect to the input list.
- `netScan` and `netFold` return pullbacks with respect to external
  parameters.
- `DiffProcess` packages differentiable initialization, state transitions, and
  output functions. `DiffSystem` packages the transition and output functions
  with state supplied externally. Their scan runners capture intermediate
  states and step pullbacks on the forward pass, then replay them in reverse.

The `circuits-stats-axioma` executable checks these interfaces. P9 compares
gradient runners with hand-derived references; P10 compares a single-input
scan with `Circuit.Diff.Circuit.quadD` from `circuits-diff` on the quadratic
`f(x) = 2x² + 3x + 5`.

## Reference

[Finite State Transducers](https://stackoverflow.com/questions/27997155/finite-state-transducers-in-haskell)
