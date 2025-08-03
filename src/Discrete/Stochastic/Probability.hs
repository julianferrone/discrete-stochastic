module Discrete.Stochastic.Probability where

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