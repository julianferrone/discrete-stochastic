module Discrete.Stochastic.Probability
  ( Prob,
    clamp,
    prob,
    almostSurely,
    almostNever,
    notP,
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

almostSurely :: Prob
almostSurely = Prob 1

almostNever :: Prob
almostNever = Prob 0

not :: Prob -> Prob
not (Prob p) = Prob $ 1 - p

(&) :: Prob -> Prob -> Prob
(Prob p1) & (Prob p2) = Prob (p1 * p2)

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