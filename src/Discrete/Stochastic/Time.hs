module Discrete.Stochastic.Time (Events (..)) where

import qualified Data.Ord as Ord
import qualified Discrete.Stochastic.SortedList as SortedList

------------------------------------------------------------
--                         Events                         --
------------------------------------------------------------

----------                  Time                  ----------

-- | Type of discrete timestep. It can be either a specific,
-- finite timestep, or a timestep infinitely far into the future.
data TimeStep = Finite Int | Infinite deriving (Eq, Ord, Show)

----------                 Events                 ----------

newtype Event a = Event {unEvent :: (Ord.Down TimeStep, a)} deriving (Eq, Ord, Show)

instance Functor Event where
  fmap f = Event . fmap f . unEvent
  (<$) f = Event . (<$) f . unEvent

-- | Events is a sequence of values, tagged with the timesteps
-- at which they happened.
newtype Events a = Events {unEvents :: SortedList.SortedList (Event a)} deriving (Eq, Show)

-- | Smart constructor that ensures the list of Events is sorted.
events :: (Ord a) => [Event a] -> Events a
events es = Events $ SortedList.fromList es

map :: (Ord b) => (a -> b) -> Events a -> Events b
map f (Events xs) = Events $ SortedList.map (fmap f) xs

instance Semigroup (Events a) where
  e1 <> e2 = Events $ unEvents e1 <> unEvents e2

instance (Ord a) => Monoid (Events a) where
  mempty = Events {unEvents = mempty}