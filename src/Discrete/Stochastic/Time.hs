module Discrete.Stochastic.Time (Events(..)) where

------------------------------------------------------------
--                         Events                         --
------------------------------------------------------------

----------                  Time                  ----------

data DiscreteTime = Finite Int | Infinite
-- DiscreteTime is a representation of a discrete timestep.
-- It can be either a specific timestep `Finite Int`, or a timestep infinitely
-- far into the future. 


----------                 Events                 ----------

newtype Events a = Events {unEvents :: [(DiscreteTime, a)]}
-- Events is a sequence of values, tagged with the timesteps 
-- at which they happened.

