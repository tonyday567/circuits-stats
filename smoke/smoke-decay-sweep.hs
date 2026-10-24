-- Decay-sweep probe for circuits-stats: a square-wave level (flips every
-- T observations, step size dA) buried in iid noise, estimated by `ma r`
-- across a sweep of decay rates, checked against the exact stationary
-- prediction from the readme §Decay weight profile.
--
-- In steady state the estimator applies weights w_k = (1-r) r^k, so for a
-- probe sitting at phase tau of its regime:
--
--   W_same(tau) = mass of w on positions in the same regime
--   bias(tau)   = dA * (W_same(tau) - 1)
--   MSE(r)      = (1/T) sum_tau bias(tau)^2  +  sigma^2 (1-r)/(1+r)
--
-- The asymptotic shortcut bias = dA * r^tau after a flip — valid only
-- while memory << T — was the first version of this probe's prediction and
-- it overestimates bias once memory approaches the regime length. The
-- phase-resolved W_same above is exact for the stationary square wave.
-- One fixed-seed noise stream serves every r (common random numbers).
--
-- Run from the repo root: runghc smoke/smoke-decay-sweep.hs

module Main where

import Circuit.Machine (scan)
import Circuit.Stats (ma)
import Circuit.Stats.Simulate (rvs)
import Data.List (minimumBy)
import Data.Ord (comparing)
import qualified Data.Vector as V
import System.Exit (ExitCode (..), exitWith)
import System.Random.MWC (initialize)
import Text.Printf (printf)

-- regime parameters
tRegime :: Int
tRegime = 200

dA :: Double
dA = 2.0 -- level step size between regimes (+1 to -1)

sigma :: Double
sigma = 0.5

nObs :: Int
nObs = 500000

burnIn :: Int
burnIn = 5000

rs :: [Double]
rs = [0.5, 0.7, 0.8, 0.9, 0.95, 0.98, 0.99, 0.995, 0.999]

levelAt :: Int -> Double
levelAt i = if even (i `div` tRegime) then 1.0 else -1.0

-- exact stationary prediction: cycle-averaged squared bias plus variance
predMse :: Double -> Double
predMse r = predBiasSq r + sigma * sigma * (1 - r) / (1 + r)
  where
    predBiasSq rr = dA * dA * sum [other tau * other tau | tau <- [0 .. tRegime - 1]] / fromIntegral tRegime
      where
        -- bias(tau) = dA * (W_same(tau) - 1) = -dA * other(tau),
        -- where other(tau) is the weight mass sitting in the old regime
        other tau = sum [w k | k <- [0 .. kMax], not (sameRegime tau k)]
        w k = (1 - rr) * rr ^ k
        kMax = ceiling (30 / negate (log rr)) -- r^k < 1e-12
        sameRegime tau k = (tau - k) `mod` (2 * tRegime) < tRegime

probe :: [Double] -> Double -> (Double, Double, Double)
probe xs r =
  let ests = scan (ma r) xs
      kept = drop burnIn (zipWith (-) ests (map levelAt [0 ..]))
      mse = sum (map (\e -> e * e) kept) / fromIntegral (length kept)
   in (mse, predMse r, relDiff mse (predMse r))

relDiff :: Double -> Double -> Double
relDiff a b = abs (a - b) / max 1e-300 b

main :: IO ()
main = do
  gen <- initialize (V.replicate 1 42)
  eps <- rvs gen nObs
  let xs = zipWith (\i e -> levelAt i + sigma * e) [0 ..] eps
      rows = map (probe xs) rs
      closeEnough = all (\(_, _, d) -> d <= 0.10) rows
      argminR f = fst (minimumBy (comparing f) (zip rs rows))
      argminMeasured = argminR (\(_, (m, _, _)) -> m)
      argminPredicted = argminR (\(_, (_, p, _)) -> p)
      argminOk = argminMeasured == argminPredicted
  putStrLn "=== decay sweep: square-wave level in iid noise ==="
  printf "T = %d  dA = %.1f  sigma = %.1f  n = %d\n" tRegime dA sigma nObs
  putStrLn "r      measured       predicted      rel diff"
  mapM_
    ( \(r, (m, p, d)) ->
        printf "%-6.3f %-14.10g %-14.10g %.4g\n" r m p d
    )
    (zip rs rows)
  putStrLn ""
  printf "argmin measured:  r = %.3f\n" argminMeasured
  printf "argmin predicted: r = %.3f\n" argminPredicted
  putStrLn (if closeEnough then "PASS measured ~= predicted (<=10% each r)" else "FAIL measured drifts from predicted")
  putStrLn (if argminOk then "PASS argmin agrees" else "FAIL argmin mismatch")
  exitWith (if closeEnough && argminOk then ExitSuccess else ExitFailure 1)
