{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-pattern-namespace-specifier #-}

-- | Ordinary differential equation integrators as streaming 'Process' machines.
--
-- A vector field is represented as a 'Diff' so the same field can be reused
-- in differentiable contexts; the integrators themselves use only the forward
-- pass.
--
-- The step size is kept separate from the state so that vector-valued states
-- (for example 'NumHask.Algebra.Metric.EuclideanPair') can be stepped with a
-- scalar step size.
--
-- The scalar self-actions for 'Double' (and 'Float') that this module used to
-- supply as orphan instances now live in "NumHask.Algebra.Action".
module Circuit.Stats.ODE
  ( -- * Vector fields
    vectorField,

    -- * Single steps
    eulerStep,
    rk4Step,

    -- * Trajectory generators
    euler,
    rk4,

    -- * Process machines
    eulerProcess,
    rk4Process,
  )
where

import Data.List (scanl')
import Circuit.Diff (Diff, runDiff, pattern Diff)
import NumHask.Prelude
import Circuit.Stats (Process (..))
import Prelude ()

-- $setup
--
-- >>> :m -Prelude
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude
-- >>> import Circuit.Stats.ODE
-- >>> import NumHask.Algebra.Metric (EuclideanPair (..))
-- >>> import Circuit.Diff (Diff, runDiff)

-- | Lift a pure vector field into a 'Diff' with zero pullback.
--
-- >>> let f = vectorField (\y -> y) :: Diff Double Double
-- >>> fst (runDiff f 2.0)
-- 2.0
vectorField :: (Additive s) => (s -> s) -> Diff s s
vectorField f = Diff $ \x -> (f x, const zero)

-- | Evaluate a differentiable vector field at a point, keeping only the
-- forward value.
evalField :: Diff s s -> s -> s
evalField f x = fst (runDiff f x)

-- | One Euler step: @y' = y + h · f(y)@.
--
-- >>> let f = vectorField (\y -> y) :: Diff Double Double
-- >>> eulerStep f 1.0 0.1
-- 1.1
eulerStep ::
  (Additive s, MultiplicativeAction s, Scalar s ~ h) =>
  Diff s s ->
  s ->
  h ->
  s
eulerStep f y h = y + (h *| evalField f y)

-- | One RK4 step.
--
-- >>> let f = vectorField (\y -> y) :: Diff Double Double
-- >>> rk4Step f 1.0 0.1
-- 1.1051708333333334
rk4Step ::
  (Additive s, Additive (Scalar s), DivisiveAction s, Scalar s ~ h) =>
  Diff s s ->
  s ->
  h ->
  s
rk4Step f y h =
  let tw = one + one
      sx = one + one + one + one + one + one
      k1 = h *| evalField f y
      k2 = h *| evalField f (y + k1 |/ tw)
      k3 = h *| evalField f (y + k2 |/ tw)
      k4 = h *| evalField f (y + k3)
   in y + (k1 + tw *| k2 + tw *| k3 + k4) |/ sx

-- | Integrate a 'Diff' vector field over a list of step sizes using Euler.
--
-- The result includes the initial state as the first element.
--
-- >>> let f = vectorField (\y -> y) :: Diff Double Double
-- >>> euler f 1.0 [0.1, 0.1, 0.1]
-- [1.0,1.1,1.2100000000000002,1.3310000000000002]
euler ::
  (Additive s, MultiplicativeAction s, Scalar s ~ h) =>
  Diff s s ->
  s ->
  [h] ->
  [s]
euler f = scanl' (eulerStep f)

-- | Integrate a 'Diff' vector field over a list of step sizes using RK4.
--
-- The result includes the initial state as the first element.
--
-- Harmonic oscillator @x'' = −x@ written as @x' = v, v' = −x@:
--
-- >>> let f = vectorField (\(EuclideanPair (x, v)) -> EuclideanPair (v, -x)) :: Diff (EuclideanPair Double) (EuclideanPair Double)
-- >>> take 5 (rk4 f (EuclideanPair (1.0, 0.0)) (replicate 40 ((pi :: Double) / 20)))
-- [EuclideanPair {euclidPair = (1.0,0.0)},EuclideanPair {euclidPair = (0.9876883614494284,-0.1564336685819834)},EuclideanPair {euclidPair = (0.9510568066766389,-0.3090154275945243)},EuclideanPair {euclidPair = (0.8910073220447337,-0.45398824664172294)},EuclideanPair {euclidPair = (0.8090185150345391,-0.5877824515637287)}]
rk4 ::
  (Additive s, Additive (Scalar s), DivisiveAction s, Scalar s ~ h) =>
  Diff s s ->
  s ->
  [h] ->
  [s]
rk4 f = scanl' (rk4Step f)

-- | A 'Process' machine that performs Euler integration.
--
-- Input is the step size @h@; output is the current state.  The first input
-- is used only to kick off the machine, so its value is ignored.
eulerProcess ::
  (Additive s, MultiplicativeAction s, Scalar s ~ h) =>
  Diff s s ->
  s ->
  Process h s
eulerProcess f y0 = Process (const y0) (eulerStep f) id

-- | A 'Process' machine that performs RK4 integration.
--
-- Input is the step size @h@; output is the current state.  The first input
-- is used only to kick off the machine, so its value is ignored.
rk4Process ::
  (Additive s, Additive (Scalar s), DivisiveAction s, Scalar s ~ h) =>
  Diff s s ->
  s ->
  Process h s
rk4Process f y0 = Process (const y0) (rk4Step f) id
