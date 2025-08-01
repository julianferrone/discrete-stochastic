module Discrete.Stochastic.Semiring (Semiring (..)) where

-- | The class of semirings (types with an additive binary operation that has
-- an identity and a multiplicative binary operation that has an identity).
--
-- Futhermore, @s@ needs to adhere to the following laws:
--
-- [Identity of Addition] @'plus' 'a' 'nil' == 'a' == 'plus' 'nil' 'a'@
-- [Commutativity of Addition] @'plus' 'a' 'b' == 'plus' 'b' 'a'@
-- [Associativity of Addition] @'plus' ('plus' 'a' 'b') 'c' == 'plus' 'a' ('plus' 'b' 'c') @
-- [Identity of Multiplication] @'times' 'a' 'unit' == 'a' == 'times' 'unit' 'a'@
-- [Associativity of Multiplication] @'times' ('times' 'a' 'b') 'c' == 'times' 'a' ('times' 'b' 'c') @
-- [Annihilation] @'times' 'nil' 'a' == 'nil' == 'times' 'a' 'nil'@
-- [Multiplication Left-Distributes over Addition] @'times' 'a' ('plus' 'b' 'c') == 'plus' ('times' 'a' 'b') ('times' 'a' 'c')@
-- [Multiplication Right-Distributes over Addition] @'times' ('plus' 'b' 'c') 'a' == 'plus' ('times' 'b' 'a') ('times' 'c' 'a')@
class Semiring s where
  nil :: s
  unit :: s
  plus :: s -> s -> s
  times :: s -> s -> s

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
