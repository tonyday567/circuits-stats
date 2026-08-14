{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-pattern-namespace-specifier #-}

-- | Reverse-mode AD through a 'Process' scan.
--
-- The idea is to run the mealy with 'NumHask.Diff.Diff' as the carrier.  Each
-- input element becomes a variable over the whole input list, so the final
-- output (or every scan output) carries a pullback wrt every input.
--
-- The high-level gradient runners 'gradScan' and 'gradFold' are the canonical
-- entry points.  They already use 'NumHask.Diff' directly, which is the same
-- primitive arrow that @circuits-ad@ builds on.  The lower-level
-- 'DiffProcess' / 'DiffSystem' reverse-step machinery is kept because its
-- explicit-state API (capture states and step pullbacks during the forward
-- pass, then replay them backward) does not have a direct, API-preserving
-- translation to @circuits-ad@'s 'Circuit.AD.Net'-based 'linearizeAt' /
-- 'Circuit.AD.backprop'.  Where a direct @circuits-ad@ replacement is
-- possible in the future, the oracles in @test/Oracle.hs@ provide regression
-- guards.
module Process.Stats.Diff
  ( -- * Gradient inputs
    GradInputs (..),
    variable,
    variables,

    -- * Reverse-step differentiable Process
    DiffProcess (..),
    StdState (..),
    diffFold,
    diffScan,

    -- * Reverse-step differentiable System (no inject; state external)
    DiffSystem (..),
    diffSystemAsDiffProcess,
    diffProcessAsDiffSystem,
    diffSystemScan,
    diffSystemFold,

    -- * Gradient-aware runners
    gradFold,
    gradScan,

    -- * Net Process runners
    constant,
    netFold,
    netScan,

    -- * Differentiable online statistics
    onlineDiff,
    maDiff,
    sqmaDiff,
    stdDiff,

    -- * Reverse-step online statistics
    onlineDiffProcess,
    maDiffProcess,
    sqmaDiffProcess,
    stdDiffProcess,

    -- * Reverse-step delay / diff
    delay1Diff,
    diffDiff,
  )
where

import Data.List (length)
import Data.Vector.Unboxed qualified as VU
import Harpie.Array (Array)
import Harpie.Array qualified as HA
import NumHask.Diff (Diff, Diff', runDiff, runDiff', pattern Diff)
import NumHask.Prelude hiding (fold, length)
import Process.Stats (Averager (..), Process, fold, ma, online, scan, sqma, std, pattern A)
import Prelude ()

-- $setup
-- >>> import Process.Stats qualified as PS
-- >>> import NumHask.Diff

-- | A length-indexed array used as the AD parameter for a scan.
--
-- The array stores one cotangent slot per input element.  Addition and
-- subtraction are pointwise over the underlying 'Harpie.Array.Array'.
newtype GradInputs a = GradInputs
  { unGradInputs :: Array a
  }
  deriving (Eq, Show)

-- | Componentwise maximum shape, padding missing dimensions with the longer
-- side.
broadcastShape :: Array a -> Array a -> [Int]
broadcastShape xs ys = go (VU.toList (HA.shape xs)) (VU.toList (HA.shape ys))
  where
    go [] bs = bs
    go as [] = as
    go (a : as) (b : bs) = max a b : go as bs

instance (Additive a) => Additive (GradInputs a) where
  zero = GradInputs (HA.konst [0] zero)
  GradInputs xs + GradInputs ys
    | HA.shape xs == HA.shape ys = GradInputs (HA.zipWith (+) xs ys)
    | otherwise =
        let s = broadcastShape xs ys
         in GradInputs (HA.zipWith (+) (HA.pad zero s xs) (HA.pad zero s ys))

instance (Subtractive a) => Subtractive (GradInputs a) where
  negate (GradInputs xs) = GradInputs (fmap negate xs)
  GradInputs xs - GradInputs ys
    | HA.shape xs == HA.shape ys = GradInputs (HA.zipWith (-) xs ys)
    | otherwise =
        let s = broadcastShape xs ys
         in GradInputs (HA.zipWith (-) (HA.pad zero s xs) (HA.pad zero s ys))

-- | A single input variable: the @i@-th element of an @n@-element input.
variable :: (Additive a) => Int -> Int -> Diff (GradInputs a) a
variable n i = Diff $ \s ->
  ( HA.index (unGradInputs s) [i],
    \da -> GradInputs (HA.modify [i] (const da) (HA.konst [n] zero))
  )

-- | Turn a list of inputs into a list of differentiable variables.
--
-- Each variable selects one array element in the forward pass and scatters a
-- cotangent back to that same position in the backward pass.
variables :: (Additive a) => [a] -> [Diff (GradInputs a) a]
variables xs =
  let n = length xs
   in [variable n i | i <- [0 .. n - 1]]

-- | Fold a list through a differentiable mealy and return the final output
-- together with a pullback through the entire fold.
--
-- >>> let m = PS.asum :: PS.Process (Diff (GradInputs Double) Double) (Diff (GradInputs Double) Double); (y, g) = gradFold m [1,2,3] in (y, g 1)
-- (6.0,[1.0,1.0,1.0])
gradFold ::
  (Additive a) =>
  Process (Diff (GradInputs a) a) (Diff (GradInputs a) b) ->
  [a] ->
  (b, b -> [a])
gradFold _ [] = error "gradFold: empty list"
gradFold m xs =
  case fold m (variables xs) of
    Nothing -> error "gradFold: empty fold"
    Just ydiff ->
      let (y, pb) = runDiff ydiff (GradInputs (HA.array [length xs] xs))
       in (y, HA.arrayAs . unGradInputs . pb)

-- | Scan a list through a differentiable mealy and return the per-step
-- outputs together with a pullback that maps a list of output cotangents
-- (one per step) to input cotangents.
--
-- >>> let m = PS.asum :: PS.Process (Diff (GradInputs Double) Double) (Diff (GradInputs Double) Double) in snd (gradScan m [1,2,3]) [1,1,1]
-- [3.0,2.0,1.0]
gradScan ::
  (Additive a) =>
  Process (Diff (GradInputs a) a) (Diff (GradInputs a) b) ->
  [a] ->
  ([b], [b] -> [a])
gradScan _ [] = ([], const [])
gradScan m xs =
  let ys = scan m (variables xs)
      n = length xs
      pbs = map (`runDiff` GradInputs (HA.array [n] xs)) ys
      z = GradInputs (HA.konst [n] zero)
      pullback dbs =
        HA.arrayAs . unGradInputs $
          foldl' (+) z [pb db | (pb, db) <- zip (snd <$> pbs) dbs]
   in (fst <$> pbs, pullback)

-- | Lift a plain input value into the parameter space of a network mealy.
--
-- The value is treated as a constant: its pullback is always zero.
constant :: (Additive p) => a -> Diff' tag p a
constant x = Diff (const (x, const zero))

-- | Fold a network mealy and return the final output together with a pullback
-- through the parameters.
netFold ::
  (Additive p) =>
  Process (Diff' tag p a) (Diff' tag p b) ->
  p ->
  [a] ->
  (b, b -> p)
netFold _ _ [] = error "netFold: empty list"
netFold m p xs =
  case fold m (map constant xs) of
    Nothing -> error "netFold: empty fold"
    Just ydiff -> runDiff' ydiff p

-- | Scan a network mealy and return the per-step outputs together with a
-- pullback that maps output cotangents to parameter cotangents.
netScan ::
  (Additive p) =>
  Process (Diff' tag p a) (Diff' tag p b) ->
  p ->
  [a] ->
  ([b], [b] -> p)
netScan _ _ [] = ([], const zero)
netScan m p xs =
  let ys = scan m (map constant xs)
      pbs = map (`runDiff'` p) ys
      pullback dbs =
        foldl' (+) zero [pb db | (pb, db) <- zip (snd <$> pbs) dbs]
   in (fst <$> pbs, pullback)

-- | 'Process.Stats.online' with a 'Diff'' carrier.
--
-- The inject and step functions now operate on differentiable values, so the
-- resulting mealy can be placed inside 'netFold' / 'netScan' and the
-- parameters inside the carrier are learned by ordinary reverse-mode AD.
onlineDiff ::
  (Additive p, Subtractive b, Divisive b) =>
  (Diff' tag p a -> Diff' tag p b) ->
  (Diff' tag p b -> Diff' tag p b) ->
  Process (Diff' tag p a) (Diff' tag p b)
onlineDiff = online

-- | Differentiable moving average with a learnable decay parameter.
maDiff ::
  (Additive p, Subtractive b, Divisive b) =>
  Diff' tag p b ->
  Process (Diff' tag p b) (Diff' tag p b)
maDiff = ma

-- | Differentiable squared moving average with a learnable decay parameter.
sqmaDiff ::
  (Additive p, Subtractive b, Divisive b) =>
  Diff' tag p b ->
  Process (Diff' tag p b) (Diff' tag p b)
sqmaDiff = sqma

-- | Differentiable standard deviation with a learnable decay parameter.
stdDiff ::
  (Subtractive p, ExpField b) =>
  Diff' tag p b ->
  Process (Diff' tag p b) (Diff' tag p b)
stdDiff = std

-- ---------------------------------------------------------------------------
-- Reverse-step Process
-- ---------------------------------------------------------------------------

-- TODO: The 'DiffProcess' / 'DiffSystem' API below is kept because its
-- explicit-state, capture-and-replay semantics do not map directly onto
-- @circuits-ad@'s 'Net'-based 'linearizeAt' while preserving the same
-- types.  A future slice could either (a) provide a 'DiffProcess'-to-'Net'
-- compiler, or (b) deprecate this API in favour of 'gradScan' / 'gradFold'
-- plus direct @circuits-ad@ 'Circuit.Net' construction.  The oracle suite
-- guards the current behaviour.

-- | A 'Process' machine with explicit state and differentiable
-- inject / step / extract functions.
--
-- The pullbacks are captured during the forward pass and then replayed by a
-- single backward walk, so the cost of a scan gradient is linear in the input
-- length (no closure-chain blow-up).
data DiffProcess s a b = DiffProcess
  { dInject :: Diff a s,
    dStep :: Diff (s, a) s,
    dExtract :: Diff s b
  }

-- | A 'System'-packaged differentiable Moore machine: state is external.
--
-- Same dynamics as 'DiffProcess', without @inject@.  Matches
-- @System s (Mono i o)@ packaging (@i@ = output, @o@ = input):
--
-- @
-- dsExtract :: Diff s i     -- output from state
-- dsStep    :: Diff (s, o) s  -- next state from state and input
-- @
--
-- Pair with an initial state via 'diffSystemAsDiffProcess' to reuse
-- 'diffScan' \/ 'diffFold'.  Agents that are 'System's sit here for Diff
-- without pretending to be 'Process' first.
data DiffSystem s i o = DiffSystem
  { dsExtract :: Diff s i,
    dsStep :: Diff (s, o) s
  }

-- | Bake in @s0@: inject is one step from that state (same as 'systemAsProcess').
--
-- @
-- diffSystemAsDiffProcess sys s0 :: DiffProcess s o i
-- @
diffSystemAsDiffProcess :: DiffSystem s i o -> s -> DiffProcess s o i
diffSystemAsDiffProcess (DiffSystem ext step) s0 =
  DiffProcess
    { dInject =
        Diff
          ( \o ->
              let (s', pb) = runDiff step (s0, o)
               in (s', \ds -> snd (pb ds))
          ),
      dStep = step,
      dExtract = ext
    }

-- | Drop inject: keep extract and step.  Initial state must be supplied
-- again when scanning (via 'diffSystemAsDiffProcess' or 'diffSystemScan').
diffProcessAsDiffSystem :: DiffProcess s a b -> DiffSystem s b a
diffProcessAsDiffSystem (DiffProcess _ step ext) = DiffSystem ext step

-- | Scan a 'DiffSystem' from external @s0@ (via 'diffSystemAsDiffProcess').
--
-- >>> let sys = diffProcessAsDiffSystem (maDiffProcess 0)
-- >>> let (ys, g) = diffSystemScan sys (A 0 0) [1,2,3] in (ys, g [1,1,1])
-- ([1.0,2.0,3.0],[1.0,1.0,1.0])
diffSystemScan ::
  (Additive s) =>
  DiffSystem s i o ->
  s ->
  [o] ->
  ([i], [i] -> [o])
diffSystemScan sys s0 = diffScan (diffSystemAsDiffProcess sys s0)

-- | Fold a 'DiffSystem' from external @s0@.
diffSystemFold ::
  (Additive s, Additive i) =>
  DiffSystem s i o ->
  s ->
  [o] ->
  (i, i -> [o])
diffSystemFold sys s0 = diffFold (diffSystemAsDiffProcess sys s0)

-- | State for a differentiable standard deviation: a moving average together
-- with a squared moving average.
data StdState a = StdState
  { stdMa :: !(Averager a a),
    stdSqMa :: !(Averager a a)
  }
  deriving (Eq, Show)

instance (Additive a) => Additive (StdState a) where
  zero = StdState zero zero
  StdState m1 s1 + StdState m2 s2 = StdState (m1 + m2) (s1 + s2)

instance (Subtractive a) => Subtractive (StdState a) where
  negate (StdState m s) = StdState (negate m) (negate s)
  StdState m1 s1 - StdState m2 s2 = StdState (m1 - m2) (s1 - s2)

-- | Forward pass: capture states and step pullbacks.
diffForward :: DiffProcess s a b -> [a] -> (s, [s], [s -> (s, a)], s -> a)
diffForward (DiffProcess inj step _) (x : xs) =
  let (s0, injPB) = runDiff inj x
      go _ [] = ([], [])
      go s (a : as) =
        let (s', pb) = runDiff step (s, a)
            (ss, pbs) = go s' as
         in (s' : ss, pb : pbs)
      (statesTail, stepPBs) = go s0 xs
   in (s0, s0 : statesTail, stepPBs, injPB)
diffForward _ [] = error "diffForward: empty list"

-- | Backward pass: walk the captured pullbacks in reverse.
diffBackward ::
  (Additive s) =>
  [s] ->
  [s -> (s, a)] ->
  (s -> a) ->
  Diff s b ->
  [b] ->
  [a]
diffBackward states stepPBs injPB ext dys =
  let n = length states
      extPBs = map (snd . runDiff ext) states
      ds0 = (extPBs !! (n - 1)) (dys !! (n - 1))
      revItems = zip3 (reverse stepPBs) (drop 1 (reverse extPBs)) (drop 1 (reverse dys))
      (dsFinal, das) =
        foldl'
          ( \(ds, acc) (pb, extPB, dy) ->
              let (dsFuture, da) = pb ds
                  dsTotal = dsFuture + extPB dy
               in (dsTotal, da : acc)
          )
          (ds0, [])
          revItems
      da0 = injPB dsFinal
   in da0 : das

-- | Scan a list through a 'DiffProcess' and return the per-step outputs together
-- with a pullback that maps output cotangents to input cotangents.
--
-- >>> let (ys, g) = diffScan (maDiffProcess 0) [1,2,3] in (ys, g [1,1,1])
-- ([1.0,2.0,3.0],[1.0,1.0,1.0])
diffScan :: (Additive s) => DiffProcess s a b -> [a] -> ([b], [b] -> [a])
diffScan _ [] = ([], const [])
diffScan m xs =
  let (_, states, stepPBs, injPB) = diffForward m xs
      ys = map (fst . runDiff (dExtract m)) states
      pullback = diffBackward states stepPBs injPB (dExtract m)
   in (ys, pullback)

-- | Fold a list through a 'DiffProcess' and return the final output together with
-- a pullback through the entire fold.
--
-- >>> let (y, g) = diffFold (maDiffProcess 0) [1,2,3] in (y, g 1)
-- (3.0,[0.0,0.0,1.0])
diffFold ::
  (Additive s, Additive b) =>
  DiffProcess s a b ->
  [a] ->
  (b, b -> [a])
diffFold m xs =
  let (ys, pullback) = diffScan m xs
   in (last ys, \dy -> pullback (replicate (length ys - 1) zero ++ [dy]))

-- | 'Process.Stats.online' as a reverse-step 'DiffProcess'.
onlineDiffProcess ::
  (Subtractive b, Divisive b) =>
  Diff a b ->
  Diff b b ->
  DiffProcess (Averager b b) a b
onlineDiffProcess f g = DiffProcess inject step extract
  where
    inject = Diff $ \x ->
      let (y, pb_f) = runDiff f x
       in (A y one, \(A ds _dc) -> pb_f ds)

    step = Diff $ \(A s c, x) ->
      let (gs, pb_gs) = runDiff g s
          (gc, pb_gc) = runDiff g c
          (fx, pb_fx) = runDiff f x
          s' = gs + fx
          c' = gc + one
          pb (A ds' dc') =
            let ds = pb_gs ds'
                dc = pb_gc dc'
                da = pb_fx ds'
             in (A ds dc, da)
       in (A s' c', pb)

    extract = Diff $ \(A s c) ->
      let y = s / c
          pb dy = A (dy / c) (-((s * dy) / (c * c)))
       in (y, pb)

-- | Differentiable moving average as a reverse-step 'DiffProcess'.
maDiffProcess ::
  (Subtractive b, Divisive b) =>
  b ->
  DiffProcess (Averager b b) b b
maDiffProcess r = onlineDiffProcess id (Diff $ \s -> (r * s, (r *)))

-- | Differentiable squared moving average as a reverse-step 'DiffProcess'.
--
-- >>> let (ys, g) = diffScan (sqmaDiffProcess 0) [1,2,3] in (ys, g [1,1,1])
-- ([1.0,4.0,9.0],[2.0,4.0,6.0])
sqmaDiffProcess ::
  (Subtractive b, Divisive b) =>
  b ->
  DiffProcess (Averager b b) b b
sqmaDiffProcess r = onlineDiffProcess square (Diff $ \s -> (r * s, (r *)))
  where
    square = Diff $ \x -> (x * x, \ds' -> (one + one) * x * ds')

-- | Differentiable standard deviation as a reverse-step 'DiffProcess'.
--
-- The gradient is taken to be zero when the standard deviation is itself zero
-- (for example, after a single sample), because the usual sqrt derivative is
-- undefined at zero.
--
-- >>> let (ys, g) = diffScan (stdDiffProcess 0) [1,2,3] in (ys, g [1,1,1])
-- ([0.0,0.0,0.0],[0.0,0.0,0.0])
stdDiffProcess ::
  (Eq b, ExpField b) =>
  b ->
  DiffProcess (StdState b) b b
stdDiffProcess r = DiffProcess inject step extract
  where
    inject = Diff $ \x ->
      let pb (StdState (A ds _dc) (A dss _dcc)) =
            ds + (one + one) * x * dss
       in (StdState (A x one) (A (x * x) one), pb)

    step = Diff $ \(StdState (A s c) (A ss cc), x) ->
      let s' = r * s + x
          c' = r * c + one
          ss' = r * ss + x * x
          cc' = r * cc + one
          pb (StdState (A ds' dc') (A dss' dcc')) =
            let ds = r * ds'
                dc = r * dc'
                dss = r * dss'
                dcc = r * dcc'
                dx = ds' + (one + one) * x * dss'
             in (StdState (A ds dc) (A dss dcc), dx)
       in (StdState (A s' c') (A ss' cc'), pb)

    extract = Diff $ \(StdState (A s c) (A ss cc)) ->
      let y = sqrt (ss / cc - (s / c) * (s / c))
          pb dy
            | y == zero = zero
            | otherwise =
                let twoY = (one + one) * y
                    dss = dy / (twoY * cc)
                    dcc = -((ss * dy) / (twoY * cc * cc))
                    ds = -((s * dy) / (y * c * c))
                    dc = (s * s * dy) / (y * c * c * c)
                 in StdState (A ds dc) (A dss dcc)
       in (y, pb)

-- | State for a one-step delay.
data DelayState a = DelayState
  { dsOut :: !a,
    dsPrev :: !a
  }
  deriving (Eq, Show)

instance (Additive a) => Additive (DelayState a) where
  zero = DelayState zero zero
  DelayState o1 p1 + DelayState o2 p2 = DelayState (o1 + o2) (p1 + p2)

instance (Subtractive a) => Subtractive (DelayState a) where
  negate (DelayState o p) = DelayState (negate o) (negate p)
  DelayState o1 p1 - DelayState o2 p2 = DelayState (o1 - o2) (p1 - p2)

-- | A one-step delay as a reverse-step 'DiffProcess'.
--
-- The initial output is @a0@; after that the machine emits the previous input.
--
-- >>> let (ys, g) = diffScan (delay1Diff 0) [1,2,3] in (ys, g [1,1,1])
-- ([0,1,2],[1,1,0])
delay1Diff :: (Additive a) => a -> DiffProcess (DelayState a) a a
delay1Diff a0 = DiffProcess inject step extract
  where
    inject = Diff $ \a -> (DelayState a0 a, dsPrev)
    step = Diff $ \(DelayState _ prev, a) ->
      let pb ds' = (DelayState zero (dsOut ds'), dsPrev ds')
       in (DelayState prev a, pb)
    extract = Diff $ \ds -> (dsOut ds, (`DelayState` zero))

-- | State for 'diffDiff'.
data DiffState a b = DiffState
  { diffOut :: !b,
    diffPrev :: !a
  }
  deriving (Eq, Show)

instance (Additive a, Additive b) => Additive (DiffState a b) where
  zero = DiffState zero zero
  DiffState o1 p1 + DiffState o2 p2 = DiffState (o1 + o2) (p1 + p2)

instance (Subtractive a, Subtractive b) => Subtractive (DiffState a b) where
  negate (DiffState o p) = DiffState (negate o) (negate p)
  DiffState o1 p1 - DiffState o2 p2 = DiffState (o1 - o2) (p1 - p2)

-- | 'Process.Stats.diff' as a reverse-step 'DiffProcess'.
--
-- The first output uses the supplied initial previous value @a0@ instead of
-- 'undefined', which makes the machine differentiable.  This is exactly the
-- log-return pattern used in @Anal.Returns.ret@.
--
-- >>> let f = Diff $ \(p, p') -> (log (p' / p), \dy -> (negate (dy / p), dy / p'))
-- >>> let (ys, g) = diffScan (diffDiff 1 f) [1,2,4]
-- >>> (ys, g [1,1,1])
-- ([0.0,0.6931471805599453,0.6931471805599453],[0.0,0.0,0.25])
diffDiff ::
  (Additive a, Additive b) =>
  a ->
  Diff (a, a) b ->
  DiffProcess (DiffState a b) a b
diffDiff a0 f = DiffProcess inject step extract
  where
    inject = Diff $ \a ->
      let (b, pb) = runDiff f (a0, a)
          pb' ds' = snd (pb (diffOut ds')) + diffPrev ds'
       in (DiffState b a, pb')
    step = Diff $ \(DiffState _ prev, a) ->
      let (b', pb) = runDiff f (prev, a)
          pb' ds' =
            let (dPrev, dA) = pb (diffOut ds')
             in (DiffState zero dPrev, dA + diffPrev ds')
       in (DiffState b' a, pb')
    extract = Diff $ \ds -> (diffOut ds, (`DiffState` zero))
