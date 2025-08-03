module Discrete.Stochastic.Probability
  ( Prob,
    clamp,
    prob,
    almostSurely,
    almostNever,
    pnot,
    Dist,
    measure,
  )
where

------------------------------------------------------------
--                       Probability                      --
------------------------------------------------------------

newtype Prob = Prob Double deriving (Eq, Ord, Show)

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

------------------------------------------------------------
--                Probability Distribution                --
------------------------------------------------------------

newtype Dist a = Dist [(Prob, a)] deriving (Eq, Ord, Show)

unDist :: Dist a -> [(Prob, a)]
unDist (Dist xs) = xs

normaliseDist :: Dist a -> Dist a
normaliseDist (Dist xs) = Dist $ map (\(p, x) -> (prob (unProb p / totalP), x)) xs
  where
    totalP :: Double
    totalP = sum . fmap (unProb . fst) $ xs

measure :: Dist a -> Prob
measure = foldr (padd . fst) almostNever . unDist

instance Functor Dist where
  fmap f (Dist xs) = Dist [(p, f x) | (p, x) <- xs]

instance Applicative Dist where
  pure x = Dist [(almostSurely, x)]
  (Dist fs) <*> (Dist xs) = Dist [(pf `ptimes` px, f x) | (pf, f) <- fs, (px, x) <- xs]

dconcat :: Dist (Dist a) -> Dist a
dconcat (Dist ds) = normaliseDist $ Dist [(pd `ptimes` px, x) | (pd, subDists) <- ds, (px, x) <- unDist subDists]

instance Monad Dist where
  return = pure
  distXs >>= f = dconcat $ fmap f distXs