{-# LANGUAGE InstanceSigs #-}

module Discrete.Stochastic.Time (Events (..)) where

import Data.List (sort)
import Data.Ord (Down, Ordering (..))

------------------------------------------------------------
--                         Events                         --
------------------------------------------------------------

----------                  Time                  ----------

-- | Type of discrete timestep. It can be either a specific,
-- finite timestep, or a timestep infinitely far into the future.
data TimeStep = Finite Int | Infinite deriving (Eq, Ord, Show)

----------                 Events                 ----------

newtype Event a = Event {unEvent :: (Down TimeStep, a)} deriving (Eq, Ord, Show)

instance Functor Event where
  fmap f = Event . fmap f . unEvent
  (<$) f = Event . (<$) f . unEvent

-- | Events is a sequence of values, tagged with the timesteps
-- at which they happened.
newtype Events a = Events {unEvents :: [Event a]} deriving (Eq, Show)

-- | Smart constructor that ensures the list of Events is sorted.
events :: (Ord a) => [Event a] -> Events a
events es = Events $ sort es

instance Functor Events where
  fmap f = Events . (fmap . fmap) f . unEvents
  (<$) f = Events . (fmap . (<$)) f . unEvents

-- | Merges two sorted lists into a sorted list.
mergeSorted :: Ord a => [a] -> [a] -> [a]
mergeSorted = go []
  where
    go :: Ord a => [a] -> [a] -> [a] -> [a]
    go done [] [] = reverse done            -- No more items to add from either input
    go done [] ys = go (done <> ys) [] []   -- No more items to add from the left input
    go done xs [] = go (done <> xs) [] []   -- No more items to add from the right input
    go done xs@(x : xtail) ys@(y : ytail)   -- Check which head is smaller
      | x <= y = go (x : done) xtail ys
      | otherwise = go (y : done) xs ytail