{-# LANGUAGE GADTs #-}

module Discrete.Stochastic.SortedList
  ( SortedList,
    toList,
    fromList,
    insert,
    merge,
    Discrete.Stochastic.SortedList.map,
  )
where

import qualified Data.List as List

data SortedList a where
  SortedList :: (Ord a) => [a] -> SortedList a

toList :: SortedList a -> [a]
toList (SortedList as) = as

fromList :: (Ord a) => [a] -> SortedList a
fromList = SortedList . List.sort

insert :: a -> SortedList a -> SortedList a
insert x (SortedList xs) = SortedList $ List.insert x xs

merge :: SortedList a -> SortedList a -> SortedList a
merge (SortedList xs) (SortedList ys) = SortedList $ mergeSorted xs ys

mergeSorted :: (Ord a) => [a] -> [a] -> [a]
mergeSorted [] ys = ys
mergeSorted xs [] = xs
mergeSorted (x : xs) (y : ys)
  | x <= y = x : mergeSorted xs (y : ys)
  | otherwise = y : mergeSorted (x : xs) ys

instance Semigroup (SortedList a) where (<>) = merge

instance (Ord a) => Monoid (SortedList a) where mempty = SortedList []

map :: (Ord b) => (a -> b) -> SortedList a -> SortedList b
map f = fromList . fmap f . toList

instance (Show a) => Show (SortedList a) where
  show (SortedList xs) = "SortedList " <> show xs