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

newtype Prob = Prob Rational deriving (Eq, Ord, Show)

clamp :: Rational -> Rational -> Rational -> Rational
clamp f c d = max f $ min c d

prob :: Rational -> Prob
prob = Prob . clamp 0 1

unProb :: Prob -> Rational
unProb (Prob p) = p

almostSurely :: Prob
almostSurely = prob 1

almostNever :: Prob
almostNever = prob 0

pnot :: Prob -> Prob
pnot (Prob p) = prob $ 1 - p

padd :: Prob -> Prob -> Prob
padd (Prob p1) (Prob p2) = prob (p1 + p2)

ptimes :: Prob -> Prob -> Prob
ptimes (Prob p1) (Prob p2) = prob (p1 * p2)

------------------------------------------------------------
--                Probability Distribution                --
------------------------------------------------------------

newtype Dist a = Dist [(a, Prob)] deriving (Eq, Ord, Show)

dist :: [(a, Prob)] -> Dist a
dist = normalise . Dist

unDist :: Dist a -> [(a, Prob)]
unDist (Dist xs) = xs

normalise :: Dist a -> Dist a
normalise (Dist xs) = Dist $ map (\(x, p) -> (x, prob (unProb p / totalP))) xs
  where
    totalP :: Rational
    totalP = sum . fmap (unProb . snd) $ xs

measure :: Dist a -> Prob
measure = foldr (padd . snd) almostNever . unDist

sampleSpace :: Dist a -> [a]
sampleSpace = fmap fst . unDist

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

coin :: Prob -> a -> a -> Dist a
coin p a b = dist [(a, p), (b, pnot p)]

uniform :: [a] -> Dist a
uniform = dist . fmap (,almostSurely)

die :: Int -> Dist Int
die n = uniform [1 .. n]