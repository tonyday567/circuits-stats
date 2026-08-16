{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-pattern-namespace-specifier #-}

-- | Oracle suite for the circuits-stats backport.
--
-- P1–P8 exercise the canonical statistical / ODE / quantile implementations.
-- P9 checks that the gradient runners in "Circuit.Stats.Diff" agree with
-- hand-derived references.  P10 cross-checks "Circuit.Stats.Diff" against
-- @circuits-ad@ on a shared quadratic computation.
module Main where

import Circuit.Diff.Circuit (Diff, data Diff, quadD, runDiff)
import Circuit.Process (Process (..), fold, scan)
import Data.List (last, sort)
import NumHask.Prelude hiding (fold, id, last)
import Circuit.Stats.Diff
  ( GradInputs,
    diffScan,
    gradFold,
    maDiffProcess,
    sqmaDiffProcess,
  )
import Circuit.Stats.ODE (euler, rk4, vectorField)
import Circuit.Stats.Quantiles (median, quantiles)
import Circuit.Stats
  ( cov,
    gdiff,
    ma,
    reg1,
    std,
  )
import System.Exit (exitFailure, exitSuccess)
import Prelude ()

main :: IO ()
main = do
  results <-
    sequence
      [ run "P1" p1,
        run "P2" p2,
        run "P3" p3,
        run "P4" p4,
        run "P5" p5,
        run "P6" p6,
        run "P7" p7,
        run "P8" p8,
        run "P9" p9,
        run "P10" p10
      ]
  if and results then exitSuccess else exitFailure

run :: String -> IO Bool -> IO Bool
run name p = do
  ok <- p
  putStrLn $ name <> ": " <> if ok then "PASS" else "FAIL"
  pure ok

approx :: Double -> Double -> Double -> Bool
approx tol x y = abs (x - y) <= tol

-- | P1: simple moving average with decay 1 is the arithmetic mean.
p1 :: IO Bool
p1 = do
  let xs = [1 .. 100 :: Double]
  pure $ fold (ma 1) xs == Just 50.5

-- | P2: standard deviation with decay 1 matches the naive closed-form
-- variance for a uniform integer grid.
p2 :: IO Bool
p2 = do
  let xs = [0 .. 1000 :: Double]
      n = fromIntegral (length xs)
      expected = sqrt ((n * n - one) / (one + one + one + one + one + one + one + one + one + one + one + one))
      actual = fromMaybe 0 $ fold (std 1) xs
  pure $ approx 1e-9 actual expected

-- | P3: online EWMA recurrence matches a hand rollout.
p3 :: IO Bool
p3 = do
  let r = 0.9 :: Double
      xs = [1, 2, 3 :: Double]
      ys = scan (ma r) xs
      hand s x = r * s + x
      s1 = hand 0 1
      s2 = hand s1 2
      s3 = hand s2 3
      expected = [s1 / 1, s2 / (r * 1 + 1), s3 / (r * (r * 1 + 1) + 1)]
  pure $ and (zipWith (approx 1e-9) ys expected)

-- | P4: covariance of a perfectly correlated pair recovers the covariance.
p4 :: IO Bool
p4 = do
  let xs = [0 .. 100 :: Double]
      pairs = zip xs (map (\x -> 2 * x + 1) xs)
      n = fromIntegral (length xs)
      varX = (n * n - one) / 12
      expected = 2 * varX
      actual = fromMaybe 0 $ fold (cov (ma 1)) pairs
  pure $ approx 1e-9 actual expected

-- | P5: regression coefficients match closed-form OLS on a noiseless line.
p5 :: IO Bool
p5 = do
  let xs = [0 .. 100 :: Double]
      pairs = zip xs (map (\x -> 2 * x + 1) xs)
      (actualAlpha, actualBeta) = fromMaybe (0, 0) $ fold (reg1 (ma 1)) pairs
  pure $ approx 1e-9 actualAlpha 1 && approx 1e-9 actualBeta 2

-- | P6: generalised diff, delay, and difference behave predictably.
p6 :: IO Bool
p6 = do
  let xs = [1, 2, 4 :: Double]
      d1 = scan (gdiff (\x -> x) (-)) xs
      d2 = scan (gdiff (const 0) (-)) xs
      d3 = scan (Process (0,) (\(_, prev) a -> (prev, a)) fst) xs
  pure $ d1 == [1, 1, 2] && d2 == [0, 1, 2] && d3 == [0, 1, 2]

-- | P7: Euler reproduces a hand recurrence; RK4 beats Euler on @y' = y@.
p7 :: IO Bool
p7 = do
  let f = vectorField (\y -> y :: Double)
      h = 0.1
      y0 = 1.0
      eulerTraj = euler f y0 (replicate 10 h)
      rk4Traj = Circuit.Stats.ODE.rk4 f y0 (replicate 10 h)
      expectedEuler = scanl (\y _ -> y + h * y) y0 (replicate 10 ())
      eulerError = abs (last eulerTraj - exp 1)
      rk4Error = abs (last rk4Traj - exp 1)
  pure $ and (zipWith (approx 1e-12) eulerTraj expectedEuler) && rk4Error < eulerError / 1000

-- | P8: quantiles / median stay bounded and quantiles are monotone in level.
p8 :: IO Bool
p8 = do
  let xs = [1 .. 100 :: Double]
      qs = [0.25, 0.5, 0.75]
      medians = scan (median 1) xs
      qss = scan (quantiles 1 qs) xs
      bounded = all (\m -> m >= 1 && m <= 100) (drop 1 medians)
      monotonic = all (\q -> sort q == q) (drop 1 qss)
  pure $ bounded && monotonic

-- | P9: gradient runners agree with hand-derived references.
p9 :: IO Bool
p9 = do
  let m = Process (\x -> x) (+) (\x -> x) :: Process (Diff (GradInputs Double) Double) (Diff (GradInputs Double) Double)
      (_, g1) = gradFold m [1, 2, 3 :: Double]
      (_, g2) = diffScan (sqmaDiffProcess 0) [1, 2, 3 :: Double]
      (_, g3) = diffScan (maDiffProcess 0) [1, 2, 3 :: Double]
  pure $ g1 1 == [1, 1, 1] && g2 [1, 1, 1] == [2, 4, 6] && g3 [1, 1, 1] == [1, 1, 1]

-- | P10: circuits-ad agrees with Circuit.Stats.Diff on a quadratic.
--
-- The shared computation is @f(x) = 2x^2 + 3x + 5@ at @x = 1@.  Both the
-- 'Circuit.Stats.Diff' scan (over a single input) and @circuits-ad@'s 'quadD'
-- should produce value 10 and gradient 7.
p10 :: IO Bool
p10 = do
  let constD :: Double -> Diff (GradInputs Double) Double
      constD x = Diff $ \_ -> (x, \_ -> zero)
      quadProcess :: Process (Diff (GradInputs Double) Double) (Diff (GradInputs Double) Double)
      quadProcess = Process (\x -> x) (\s _ -> s) (\s -> constD 2 * s * s + constD 3 * s + constD 5)
      (yDiff, gDiff) = gradFold quadProcess [1 :: Double]
      (yAD, pbAD) = runDiff quadD 1
      gradDiff = case gDiff 1 of
        (g : _) -> g
        [] -> 0 / 0
      gradAD = pbAD 1
  pure $ approx 1e-9 yDiff yAD && approx 1e-9 gradDiff gradAD && approx 1e-9 yDiff 10
