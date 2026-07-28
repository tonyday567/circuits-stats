# process-stats

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
import Process.Stats
```

```haskell
fold ((,) <$> ma 0.9 <*> std 0.9) [1..100::Double]
```

```
(91.00265621044142,9.472822289121)
```

## Reference

[Finite State Transducers](https://stackoverflow.com/questions/27997155/finite-state-transducers-in-haskell)
