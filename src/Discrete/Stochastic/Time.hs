module Discrete.Stochastic.Time () where

------------------------------------------------------------
--                         Events                         --
------------------------------------------------------------

----------                  Time                  ----------

data DiscreteTime = Finite Int | Infinite

----------                 Events                 ----------

newtype Events a = Events {unEvents :: [(DiscreteTime, a)]}
