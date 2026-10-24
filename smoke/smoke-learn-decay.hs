-- Learn the decay rate by reverse mode through the scan (stage 3 of
-- timescale-decay): track the square-wave level from
-- smoke-decay-sweep.hs, but instead of sweeping r, learn it by gradient
-- descent on a short train prefix via netScan over maDiff (the exact AD
-- gradient), then check the learned rate against the sweep argmin
-- measured on a long held-out half.
--
-- Cost note: the netScan gradient lane composes pullback closures per
-- step, so one gradient is quadratic in the stream length; the train
-- prefix is kept short (2k observations) for that reason. The linear
-- reverse-step lane (maDiffProcess) hardcodes the decay as a plain value
-- with zero pullback, so it cannot learn r — a documented trade-off of
-- the library's two Diff lanes, pinned here by use.
--
-- Verdict: the meaningful criterion is held-out MSE, not |dr|. The
-- predicted MSE curve is a plateau across r in [0.7, 0.8] (values differ
-- by ~0.1%), so the train-window argmin wanders inside it and dr against
-- any single sweep point is noise. PASS = learned test MSE within 1% of
-- the sweep argmin's; dr is reported as information, not gated.
--
-- Run from the repo root: runghc smoke/smoke-learn-decay.hs

module Main where

import Circuit.Machine (scan)
import Circuit.Stats (ma)
import Circuit.Stats.Diff (maDiff, netScan)
import Circuit.Stats.Simulate (rvs)
import qualified Control.Category as Cat
import qualified Data.Vector as V
import System.Exit (ExitCode (..), exitWith)
import System.Random.MWC (initialize)
import Text.Printf (printf)

tRegime :: Int
tRegime = 200

dA :: Double
dA = 2.0

sigma :: Double
sigma = 0.5

nTrain :: Int
nTrain = 2000 -- short: the netScan gradient is quadratic in stream length

nTest :: Int
nTest = 100000

burnIn :: Int
burnIn = 200

rs :: [Double]
rs = [0.5, 0.7, 0.8, 0.9, 0.95, 0.98, 0.99, 0.995, 0.999]

steps :: Int
steps = 50

lr :: Double
lr = 0.1

levelAt :: Int -> Double
levelAt i = if even (i `div` tRegime) then 1.0 else -1.0

-- cycle-averaged MSE of ma r against the true level on a stream segment
trackingMse :: [Double] -> Int -> Double -> Double
trackingMse xs i0 r =
  let ests = scan (ma r) xs
      kept = drop burnIn (zipWith (\i e -> e - levelAt (i0 + i)) [0 ..] ests)
   in sum (map (\e -> e * e) kept) / fromIntegral (length kept)

-- exact gradient of the tracking loss w.r.t. the decay, via netScan
decayGrad :: [Double] -> Int -> Double -> Double
decayGrad xs i0 r =
  let (ests, pull) = netScan (maDiff Cat.id) r xs
      nKept = fromIntegral (length xs - burnIn) :: Double
      dbs =
        [ if i < burnIn
            then 0
            else 2 * (e - levelAt (i0 + i)) / nKept
        | (i, e) <- zip [0 ..] ests
        ]
   in pull dbs

main :: IO ()
main = do
  gen <- initialize (V.replicate 1 42)
  eps <- rvs gen (nTrain + nTest)
  let xs = zipWith (\i e -> levelAt i + sigma * e) [0 ..] eps
      (train, test) = splitAt nTrain xs
      learn r trace 0 = (r, trace)
      learn r traceLeft k =
        let g = decayGrad train 0 r
            r' = r - lr * g
         in learn r' ((k, r, g) : traceLeft) (k - 1 :: Int)
      (learned, traceRev) = learn 0.5 [] steps
      trace = reverse traceRev
      sweepOnTest = [(r, trackingMse test nTrain r) | r <- rs]
      argminTest = fst (minimumBy' snd sweepOnTest)
      mseLearned = trackingMse test nTrain learned
      mseArgmin = trackingMse test nTrain argminTest
      mseOk = mseLearned <= mseArgmin * 1.01
  putStrLn "=== learn the decay rate through the scan ==="
  printf "T = %d  dA = %.1f  sigma = %.1f  train = %d  test = %d  lr = %.2f  steps = %d\n" tRegime dA sigma nTrain nTest lr steps
  putStrLn "step   r            dL/dr"
  mapM_ (\(k, r, g) -> printf "%-6d %-12.8g %.4g\n" (steps - k :: Int) r g) (decimate trace)
  putStrLn ""
  printf "learned r:        %.6f\n" learned
  printf "sweep argmin r:   %.6f (on held-out half)\n" argminTest
  printf "|dr|:             %.6f (informational; the r-curve is a plateau)\n" (abs (learned - argminTest))
  printf "test MSE learned: %.10g\n" mseLearned
  printf "test MSE argmin:  %.10g\n" mseArgmin
  putStrLn (if mseOk then "PASS learned test MSE within 1% of argmin" else "FAIL learned test MSE above argmin")
  exitWith (if mseOk then ExitSuccess else ExitFailure 1)
  where
    minimumBy' f = foldl1 (\a b -> if f a <= f b then a else b)
    decimate = filter (\(k, _, _) -> k `mod` 5 == 0)
