{-# LANGUAGE InstanceSigs #-}

module Discrete.Stochastic.Time (Events (..)) where

import Data.Ord (Down, Ordering (..))

------------------------------------------------------------
--                         Events                         --
------------------------------------------------------------

----------                  Time                  ----------

-- | Type of discrete timestep. It can be either a specific,
-- finite timestep, or a timestep infinitely far into the future.
data TimeStep = Finite Int | Infinite deriving (Eq, Ord, Show)

----------                 Events                 ----------

newtype Event a = Event {unEvent :: (Down TimeStep, a)} deriving (Eq, Show)

instance Functor Event where
  fmap f = Event . fmap f . unEvent
  (<$) f = Event . (<$) f . unEvent

-- | Events is a sequence of values, tagged with the timesteps
-- at which they happened.
newtype Events a = Events {unEvents :: [Event a]} deriving (Eq, Show)

instance Functor Events where
  fmap f = Events . (fmap . fmap) f . unEvents
  (<$) f = Events . (fmap . (<$)) f . unEvents

-- instance Semigroup (Events a) where
--   earlier <> later = Events $ unEvents earlier <> unEvents later
