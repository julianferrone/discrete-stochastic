{-# LANGUAGE InstanceSigs #-}

module Discrete.Stochastic.Time
  ( Time (..),
    epoch,
    previous,
    next,
    Duration (..),
    after,
    to,
    Observable,
    konst,
    sample,
    timeTo,
  )
where

import Discrete.Stochastic.Semiring

------------------------------------------------------------
--                         Events                         --
------------------------------------------------------------

----------                  Time                  ----------

-- | The type for a discrete point in time. A value of type @'Time'@
-- is either some finite point in time @Finite Int@, or it is the time
-- infinitely far into the future @Infinite@.
data Time = Finite Int | Infinite deriving (Eq, Ord, Show)

instance Semiring Time where
  nil = epoch
  unit = Infinite
  plus = max
  times = min

-- | Returns the reference point of the @Time@ type.
epoch :: Time
epoch = Finite 0

-- | Returns the previous time @Just Time@ before a given @Time@, if the
-- provided time is finite and after the epoch. Otherwise, returns @Nothing@.
previous :: Time -> Maybe Time
previous (Finite t) | t > 0 = Just . Finite $ t - 1
previous _ = Nothing

-- | Returns the next `Time` after a `Time`-value.
next :: Time -> Time
next (Finite n) = Finite (n + 1)
next Infinite = Infinite

-- | The type for a discrete time interval. A @Duration@ is either some
-- a finite @Duration@ between two @Finite@ points in time, or an @Indeterminate@
-- duration involving at least one point in time infinitely far into the future
-- (@Infinite@.)
data Duration = Duration Int | Indeterminate deriving (Eq, Ord, Show)

instance Semigroup Duration where
  (Duration d1) <> (Duration d2) = Duration $ d1 + d2
  _ <> _ = Indeterminate

instance Monoid Duration where
  mempty = Duration 0

-- | Adds a @Duration@ to a @Time@.
--
-- __Examples__
--
-- >>> Duration 5 `after` Finite 10
-- Finite 15
-- >>> Indeterminate `after` Finite 1
-- Infinite
-- >>> Duration 5 `after` Infinite
-- Infinite
after :: Duration -> Time -> Time
after (Duration d) (Finite t) = Finite $ d + t
after _ _ = Infinite

-- | Finds the @Duration@ between two points in time @Time@.
--
-- __Examples__
--
-- >>> Finite 5 `to` Finite 10
-- Duration 5
-- >>> Finite 3 `to` epoch
-- Duration (-3)
-- >>> Finite 5 `to` Infinite
-- Indeterminate
-- >>> Infinite `to` Finite 10
-- Indeterminate
to :: Time -> Time -> Duration
to (Finite t1) (Finite t2) = Duration $ t2 - t1
to _ _ = Indeterminate

----------             Time Processes             ----------

-- | The @Observable a@ type represents a time-varying quantity of type @a@.
newtype Observable a = Observable (Time -> a)

-- | Samples an @Observable a@ at a given @Time@, returning a quantity of type
-- @a@.
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

instance (Semiring r) => Semiring (Observable r) where
  nil = konst nil
  unit = konst unit
  plus (Observable f) (Observable g) = Observable $ \t -> f t `plus` g t
  times (Observable f) (Observable g) = Observable $ \t -> f t `times` g t

-- | Creates an @Observable a@ that is sampled to the same value over all 
-- @Time@s.
--
-- __Examples__
--
-- >>> sample (konst 10) (Finite 1)
-- 10
-- >>> sample (konst 5) Infinite
-- 5
konst :: a -> Observable a
konst = Observable . const

-- | The sample of the observable @(timeTo t)@ at time @s@ is the duration
-- between @s@ and @t@, positive if @s@ is earlier than @t@.
--
-- __Examples__
--
-- >>> let s = Finite 2
-- >>> let t = Finite 10
-- >>> sample (timeTo t) s
-- Duration 8
timeTo :: Time -> Observable Duration
timeTo t1 = Observable $ \t2 -> t2 `to` t1
