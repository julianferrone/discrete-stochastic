module Discrete.Stochastic.Probability where

------------------------------------------------------------
--                       Probability                      --
------------------------------------------------------------

newtype Prob = Prob Double

clamp :: Double -> Double -> Double -> Double
clamp floor ceiling d = max floor $ min ceiling d

prob :: Double -> Prob
prob = Prob . clamp 0 1

------------------------------------------------------------
--                Probability Distribution                --
------------------------------------------------------------