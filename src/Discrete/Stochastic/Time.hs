{-# LANGUAGE InstanceSigs #-}

module Discrete.Stochastic.Time () where

import qualified Data.Ord as Ord

------------------------------------------------------------
--                         Events                         --
------------------------------------------------------------

----------               Typeclasses              ----------

------ Semiring

class Semiring r where
  nil :: r
  unit :: r
  plus :: r -> r -> r
  times :: r -> r -> r

instance Semiring Int where
  nil = 0
  unit = 1
  plus = (+)
  times = (*)

instance Semiring Double where
  nil = 0
  unit = 1
  plus = (+)
  times = (*)

instance (Semiring b) => Semiring (a -> b) where
  nil = const nil
  unit = const unit
  plus f g x = f x `plus` g x
  times f g x = f x `times` g x

----------                  Time                  ----------

-- | Type of discrete timestep. It can be either a specific,
-- finite timestep, or a timestep infinitely far into the future.
data Time = Finite Int | Infinite deriving (Eq, Ord, Show)

instance Semiring Time where
  nil = epoch
  unit = Infinite
  plus = max
  times = min

epoch :: Time
epoch = Finite 0

previous :: Time -> Maybe Time
previous (Finite t) | t > 0 = Just $ Finite $ t - 1
previous _ = Nothing

next :: Time -> Time
next (Finite n) = Finite (n + 1)
next Infinite = Infinite

diff :: Time -> Time -> Maybe Int
diff (Finite t1) (Finite t2) = Just $ t2 - t1
diff _ _ = Nothing

----------             Time Processes             ----------

newtype Observable a = Observable (Time -> a)

instance Functor Observable where
  fmap f (Observable g) = Observable $ f . g

instance Applicative Observable where
  pure = konst
  (<*>) :: Observable (a -> b) -> Observable a -> Observable b
  Observable f <*> Observable g = Observable $ \t -> f t (g t)

instance (Semiring r) => Semiring (Observable r) where
  nil = konst nil
  unit = konst unit
  plus (Observable f) (Observable g) = Observable $ \t -> f t `plus` g t
  times (Observable f) (Observable g) = Observable $ \t -> f t `times` g t

konst :: a -> Observable a
konst = Observable . const