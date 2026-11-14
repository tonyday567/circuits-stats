{-# LANGUAGE RebindableSyntax #-}

-- | Exact finite-sample and approximate streaming quantile statistics.
module Circuit.Stats.Quantiles
  ( median,
    quantiles,
    exactMedian,
    exactQuantile,
    exactQuantiles,
    digitize,
    signalize,
    OnlineTDigest (..),
    emptyOnlineTDigest,
    onlineInsert,
    onlineCompress,
    onlineForceCompress,
  )
where

import Circuit.Stats
import Data.List (sort)
import Data.Maybe (listToMaybe)
import Data.Sketch.TDigest qualified as TD
import NumHask.Prelude hiding (fold)

-- $setup
-- >>> import Prelude

-- | An online t-digest with exponential decay weighting.
--
-- Each inserted point receives weight @r ** (-(n+1))@ where @n@ counts
-- insertions since the last forced compression. Periodically the weights
-- are rescaled so that older points decay relative to newer ones.
data OnlineTDigest = OnlineTDigest
  { td :: TD.TDigest,
    tdN :: Int,
    tdRate :: Double
  }
  deriving (Show)

-- | T-digest compression parameter. Delta=50 reproduces the accuracy profile
-- of the previous @tdigest@ package at its compression=25 setting.
tdigestDelta :: Double
tdigestDelta = 50

emptyOnlineTDigest :: Double -> OnlineTDigest
emptyOnlineTDigest = OnlineTDigest (TD.emptyWith tdigestDelta) 0

-- | Exact finite-sample median using the upper middle order statistic for an
-- even number of observations.
--
-- >>> exactMedian ([] :: [Int])
-- Nothing
-- >>> exactMedian [7 :: Int]
-- Just 7
-- >>> exactMedian [3, 1, 2 :: Int]
-- Just 2
-- >>> exactMedian [4, 1, 3, 2 :: Int]
-- Just 3
exactMedian :: (Ord a) => [a] -> Maybe a
exactMedian = exactQuantile 0.5

-- | Exact finite-sample quantile from an observed order statistic.
--
-- For @n@ sorted, zero-based observations, probability @p@ selects index
-- @min (n - 1) (floor (p * n))@. Thus @p = 0@ selects the minimum, @p = 1@
-- selects the maximum, and @p = 0.5@ selects the upper middle observation
-- when @n@ is even. Every result is an observed sample element.
--
-- Returns 'Nothing' for an empty sample or when @p@ is outside @[0,1]@.
-- NaN is invalid.
--
-- >>> exactQuantile 0.5 ([] :: [Int])
-- Nothing
-- >>> exactQuantile (-0.1) [1, 2, 3 :: Int]
-- Nothing
-- >>> exactQuantile 1.1 [1, 2, 3 :: Int]
-- Nothing
-- >>> exactQuantile (0 / 0) [1, 2, 3 :: Int]
-- Nothing
-- >>> exactQuantile 0 [3, 1, 2 :: Int]
-- Just 1
-- >>> exactQuantile 1 [3, 1, 2 :: Int]
-- Just 3
exactQuantile :: (Ord a) => Double -> [a] -> Maybe a
exactQuantile p xs = exactQuantiles [p] xs >>= listToMaybe

-- | Exact finite-sample quantiles, sorting the sample once for all requested
-- probabilities. Each result follows 'exactQuantile'.
--
-- Returns 'Nothing' for an empty sample, or if any probability is outside
-- @[0,1]@ or NaN; an invalid probability rejects the whole batch. For a
-- nonempty sample, an empty probability list returns @Just []@.
--
-- The p10, p50, and p90 convention matches @perf-explore@'s index-based
-- reporting:
--
-- >>> exactQuantiles [0.1, 0.5, 0.9] [1 .. 10 :: Int]
-- Just [2,6,10]
-- >>> exactQuantiles [0.1, 0.5, 0.9] [10, 9 .. 1 :: Int]
-- Just [2,6,10]
-- >>> exactQuantiles [0.5] [2, 1, 2, 2, 1 :: Int]
-- Just [2]
-- >>> exactQuantiles [0.5, 1.1] [1 .. 10 :: Int]
-- Nothing
exactQuantiles :: (Ord a) => [Double] -> [a] -> Maybe [a]
exactQuantiles ps xs
  | null xs = Nothing
  | all validProbability ps = Just $ select <$> ps
  | otherwise = Nothing
  where
    sorted = sort xs
    n = length sorted
    select p = sorted !! min (n - 1) (floor (p * fromIntegral n))
    validProbability p = p >= 0 && p <= 1

-- | Moore quantiles based on the t-digest library.
quantiles :: Double -> [Double] -> MooreT (,) (->) Double [Double]
quantiles r qs = moore inject step extract
  where
    step x a = onlineInsert a x
    inject a = onlineInsert a (emptyOnlineTDigest r)
    extract x = fromMaybe (0 / 0) . (`TD.quantile` t) <$> qs
      where
        (OnlineTDigest t _ _) = onlineForceCompress x

-- | Moore median using the t-digest algorithm.
--
-- The t-digest algorithm works best at extremes and can be unreliable in the centre.
median :: Double -> MooreT (,) (->) Double Double
median r = moore inject step extract
  where
    step x a = onlineInsert a x
    inject a = onlineInsert a (emptyOnlineTDigest r)
    extract x = fromMaybe (0 / 0) (TD.quantile 0.5 t)
      where
        (OnlineTDigest t _ _) = onlineForceCompress x

onlineInsert' :: Double -> OnlineTDigest -> OnlineTDigest
onlineInsert' x (OnlineTDigest td' n r) =
  OnlineTDigest
    (TD.addWeighted x (r ^^ (-(fromIntegral $ n + 1) :: Integer)) td')
    (n + 1)
    r

onlineInsert :: Double -> OnlineTDigest -> OnlineTDigest
onlineInsert x otd = onlineCompress (onlineInsert' x otd)

-- | Force a compression pass when the digest has grown enough that the
-- decay weights are becoming numerically awkward. The threshold is chosen
-- to keep weights within a comfortable double range while preserving the
-- exponential-decay semantics.
onlineCompress :: OnlineTDigest -> OnlineTDigest
onlineCompress otd@(OnlineTDigest _ n _)
  | n > maxInsertsSinceCompress = onlineForceCompress otd
  | otherwise = otd

maxInsertsSinceCompress :: Int
maxInsertsSinceCompress = 200

-- | Rescale all centroid weights by @r ** n@ and reset the insertion counter.
-- This is the exponential-decay equivalent of normalising weights.
onlineForceCompress :: OnlineTDigest -> OnlineTDigest
onlineForceCompress (OnlineTDigest t n r) = OnlineTDigest t' 0 r
  where
    t' = TD.compress $ foldl' rebuild (TD.emptyWith tdigestDelta) (TD.centroidList t)
    rebuild a c = TD.addWeighted (TD.cMean c) (TD.cWeight c * r ^^ n) a

-- | A process that computes the running quantile bucket. For example,
-- in a scan, @digitize 0.9 [0.5]@ returns:
--
-- * 0 if the current value is less than the current process median.
--
-- * 1 if the current value is greater than the current process median.
digitize :: Double -> [Double] -> MooreT (,) (->) Double Int
digitize r qs = moore inject step extract
  where
    step (x, _) a = (onlineInsert a x, a)
    inject a = (onlineInsert a (emptyOnlineTDigest r), a)
    extract (x, l) = bucket' qs' l
      where
        qs' = fromMaybe (0 / 0) . (`TD.quantile` t) <$> qs
        (OnlineTDigest t _ _) = onlineForceCompress x
        bucket' xs l' =
          fromMaybe 0
            . fold (moore id (+) id)
            $ ( \x' ->
                  if x' > l'
                    then 0
                    else 1
              )
              <$> xs

-- | transform an input to a [0,1] signal, via digitalization.
signalize :: Double -> [Double] -> MooreT (,) (->) Double Double
signalize r qs' =
  fmap (\x -> fromIntegral x / fromIntegral (length qs' + 1)) (digitize r qs')
