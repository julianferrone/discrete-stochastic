module Discrete.Stochastic.Time (Events (..)) where

------------------------------------------------------------
--                         Events                         --
------------------------------------------------------------

----------                  Time                  ----------

-- | Type of discrete timestep. It can be either a specific,
-- finite timestep, or a timestep infinitely far into the future.
data TimeStep = Finite Int | Infinite deriving (Eq, Ord, Show)

----------                 Events                 ----------

-- | Events is a sequence of values, tagged with the timesteps
-- at which they happened.
newtype Events a = Events {unEvents :: [(TimeStep, a)]} deriving (Eq, Show)

instance Functor Events where
  fmap f = Events . (fmap . fmap) f . unEvents
  (<$) f = Events . (fmap . (<$)) f . unEvents
