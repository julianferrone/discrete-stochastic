module Discrete.Stochastic.Probability where

------------------------------------------------------------
--                       Probability                      --
------------------------------------------------------------

newtype Prob = Prob Double

clamp :: Double -> Double -> Double -> Double
clamp f c d = max f $ min c d

prob :: Double -> Prob
prob = Prob . clamp 0 1

------------------------------------------------------------
--                Probability Distribution                --
------------------------------------------------------------