{-# LANGUAGE TupleSections #-}

module Discrete.Stochastic.Probability
  ( Prob,
    clamp,
    prob,
    almostSurely,
    almostNever,
    pnot,
    Dist,
    dist,
    sampleSpace,
    combineOutcomes,
    measure,
    coin,
    uniform,
    die,
  )
where

import qualified Data.Map as Map

------------------------------------------------------------
--                       Probability                      --
------------------------------------------------------------

-- | The type of probabilities, which must be between 0 and 1 (inclusive).
newtype Prob = Prob Rational deriving (Eq, Ord, Show)

-- | Clamps a rational number between a floor and a ceiling
clamp :: Rational -> Rational -> Rational -> Rational
clamp f c r = max f $ min c r

-- | Smart constructor for @Prob@s. Enforces that the probability of an outcome
-- is always between 0 and 1 (inclusive).
prob :: Rational -> Prob
prob = Prob . clamp 0 1

-- | Extracts the underlying @Rational@ from a @Prob@.
unProb :: Prob -> Rational
unProb (Prob p) = p

-- | The probability that an event will almost surely happen i.e. with
-- probability 1.
almostSurely :: Prob
almostSurely = prob 1

-- | The probability that an event will almost never happen i.e. with
-- probability 0.
almostNever :: Prob
almostNever = prob 0

-- | Probability "not".
--
-- __Examples__
--
-- >>> pnot (prob 0.6)
-- prob 0.4
pnot :: Prob -> Prob
pnot (Prob p) = prob $ 1 - p

-- | Adds two probabilities together.
--
-- This is not the probability of either event occurring, unless they are
-- mutually exclusive.
padd :: Prob -> Prob -> Prob
padd (Prob p1) (Prob p2) = prob (p1 + p2)

-- | Multiplies two probabilities together.
--
-- This is not the probability of both events occurring, unless they are
-- independent events
ptimes :: Prob -> Prob -> Prob
ptimes (Prob p1) (Prob p2) = prob (p1 * p2)

------------------------------------------------------------
--                Probability Distribution                --
------------------------------------------------------------

-- | The type of discrete probability distributions.
newtype Dist a = Dist [(a, Prob)] deriving (Eq, Ord, Show)

-- | Smart constructor for a @Dist@, enforcing that the sum of all
-- probabilities in the distribution must equal to 1.
dist :: [(a, Prob)] -> Dist a
dist = normalise . Dist

-- | Unwraps a @Dist@ into a list of probability-tagged events
-- @[(a, Prob)]@.
unDist :: Dist a -> [(a, Prob)]
unDist (Dist xs) = xs

-- | Normalises a @Dist@, enforcing that the sum of all probabilities in the
-- @Dist@ is equal to 1.
normalise :: Dist a -> Dist a
normalise (Dist xs) = Dist $ map (\(x, p) -> (x, prob (unProb p / totalP))) xs
  where
    totalP :: Rational
    totalP = sum . fmap (unProb . snd) $ xs

-- | Returns the probability measure of a @Dist@, which should always be equal
-- to 1.
measure :: Dist a -> Prob
measure = foldr (padd . snd) almostNever . unDist

-- | Returns the sample space of a @Dist@, that is to say the set (returned as
-- a @List@) of all possible outcomes of the trial encoded in the @Dist@.
sampleSpace :: Dist a -> [a]
sampleSpace = fmap fst . unDist

-- | Deduplicates identical outcomes in a @Dist@.
combineOutcomes :: (Ord a) => Dist a -> Dist a
combineOutcomes = dist . Map.toList . Map.fromListWith padd . unDist

instance Functor Dist where
  fmap f (Dist xs) = Dist [(f x, p) | (x, p) <- xs]

instance Applicative Dist where
  pure x = Dist [(x, almostSurely)]
  (Dist fs) <*> (Dist xs) = Dist [(f x, pf `ptimes` px) | (f, pf) <- fs, (x, px) <- xs]

dconcat :: Dist (Dist a) -> Dist a
dconcat (Dist ds) =
  normalise $
    Dist
      [ (x, pd `ptimes` px)
        | (subDists, pd) <- ds,
          (x, px) <- unDist subDists
      ]

instance Monad Dist where
  return = pure
  distXs >>= f = dconcat $ fmap f distXs

-- | Creates a probability distribution for a weighted coin.
--
-- __Examples__
--
-- >>> coin (prob 0.6) True False
-- Dist [(True, Prob (3 % 5)), (False, Prob (2 % 5))]
coin :: Prob -> a -> a -> Dist a
coin p a b = dist [(a, p), (b, pnot p)]

-- | Creates a uniform distribution for a list of values.
--
-- __Examples__
--
-- >>> uniform ["a", "b", "c"]
-- Dist [("a", Prob (1 % 3)), ("b", Prob (1 % 3)), ("c", Prob (1 % 3))]
uniform :: [a] -> Dist a
uniform = dist . fmap (,almostSurely)

-- | Creates a uniform distribution for a fair n-sided die.
--
-- __Examples__
--
-- >>> die 6
-- Dist [
--     (1, Prob (1 % 6)),
--     (2, Prob (1 % 6)),
--     (3, Prob (1 % 6)),
--     (4, Prob (1 % 6)),
--     (5, Prob (1 % 6)),
--     (6, Prob (1 % 6))
--   ]
die :: Int -> Dist Int
die n = uniform [1 .. n]