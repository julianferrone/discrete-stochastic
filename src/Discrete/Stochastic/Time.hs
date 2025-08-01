module Discrete.Stochastic.Time () where

import qualified Data.Ord as Ord

------------------------------------------------------------
--                         Events                         --
------------------------------------------------------------

----------               Typeclasses              ----------

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
data TimeStep = Finite Int | Infinite deriving (Eq, Ord, Show)

