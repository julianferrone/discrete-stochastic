module Discrete.Stochastic.Time () where

import qualified Data.Ord as Ord

------------------------------------------------------------
--                         Events                         --
------------------------------------------------------------

----------                  Time                  ----------

-- | Type of discrete timestep. It can be either a specific,
-- finite timestep, or a timestep infinitely far into the future.
data TimeStep = Finite Int | Infinite deriving (Eq, Ord, Show)

