{-# LANGUAGE InstanceSigs #-}

module Discrete.Stochastic.Time
  ( Semiring (..),
    Time (..),
    Duration (..),
    Observable,
    konst,
    sample,
    timeTo,
  )
where

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

-- | Discrete time point. Can be either a `Finite` time, or a time infinitely far
-- into the future `Infinite`.
data Time = Finite Int | Infinite deriving (Eq, Ord, Show)

instance Semiring Time where
  nil = epoch
  unit = Infinite
  plus = max
  times = min

epoch :: Time
epoch = Finite 0

previous :: Time -> Maybe Time
previous (Finite t) | t > 0 = Just . Finite $ t - 1
previous _ = Nothing

next :: Time -> Time
next (Finite n) = Finite (n + 1)
next Infinite = Infinite

-- | A discrete time step. Can be either a finite `Duration` (when taking the
-- difference between two Finite times), or `Indeterminate` (when adding or
-- subtracing an Infinite time).
data Duration = Duration Int | Indeterminate deriving (Eq, Ord, Show)

after :: Duration -> Time -> Time
after (Duration d) (Finite t) = Finite $ d + t
after _ _ = Infinite

to :: Time -> Time -> Duration
to (Finite t1) (Finite t2) = Duration $ t2 - t1
to _ _ = Indeterminate

----------             Time Processes             ----------

newtype Observable a = Observable (Time -> a)

sample :: Observable a -> Time -> a
sample (Observable f) = f

instance Functor Observable where
  fmap f (Observable g) = Observable $ f . g

instance Applicative Observable where
  pure = konst
  (<*>) :: Observable (a -> b) -> Observable a -> Observable b
  Observable f <*> Observable g = Observable $ \t -> f t (g t)

instance Monad Observable where
  (>>=) :: Observable a -> (a -> Observable b) -> Observable b
  o >>= f = Observable $ f . sample o >>= sample

-- (>>=) :: (Time -> a) -> (a -> (Time -> b)) -> (Time -> b)

instance (Semiring r) => Semiring (Observable r) where
  nil = konst nil
  unit = konst unit
  plus (Observable f) (Observable g) = Observable $ \t -> f t `plus` g t
  times (Observable f) (Observable g) = Observable $ \t -> f t `times` g t

konst :: a -> Observable a
konst = Observable . const

timeTo :: Time -> Observable Duration
timeTo t1 = Observable $ \t2 -> t2 `to` t1
