module Discrete.Stochastic.Probability
  ( Prob,
    clamp,
    prob,
    almostSurely,
    almostNever,
    pnot,
    Dist,
  )
where

------------------------------------------------------------
--                       Probability                      --
------------------------------------------------------------

newtype Prob = Prob Double

clamp :: Double -> Double -> Double -> Double
clamp f c d = max f $ min c d

prob :: Double -> Prob
prob = Prob . clamp 0 1

unProb :: Prob -> Double
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

pdiv :: Prob -> Prob -> Prob
pdiv (Prob p1) (Prob p2) = prob (p1 / p2)

------------------------------------------------------------
--                Probability Distribution                --
------------------------------------------------------------

newtype Dist a = Dist [(Prob, a)]

unDist :: Dist a -> [(Prob, a)]
unDist (Dist xs) = xs

instance Functor Dist where
  fmap f (Dist xs) = Dist [(p, f x) | (p, x) <- xs]

instance Applicative Dist where
  pure x = Dist [(almostSurely, x)]
  (Dist fs) <*> (Dist xs) = Dist [(pf & px, f x) | (pf, f) <- fs, (px, x) <- xs]

dconcat :: Dist (Dist a) -> Dist a
dconcat (Dist ds) = Dist [(pd & px, x) | (pd, subDists) <- ds, (px, x) <- unDist subDists]

instance Monad Dist where
  return = pure
  distXs >>= f = dconcat $ fmap f distXs