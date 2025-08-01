module Discrete.Stochastic.Semiring (Semiring (..)) where

-- | A type @s@ is a Semiring if it provides:
-- - An addition operation @plus@ which, given two elements @s@, returns another element @s@
-- - A multiplication operation @times@ which, given two elements @s@, returns another element @s@
-- - An identity element for addition @nil@
-- - An identity element for multiplication @unit@
--
-- Futhermore, @s@ needs to adhere to the following laws:
--
-- [Identity Of Addition] @'plus' 'a' 'nil' == 'a' == 'plus' 'nil' 'a'@
-- [Commutativity Of Addition] @'plus' 'a' 'b' == 'plus' 'b' 'a'@
-- [Associativity Of Addition] @'plus' ('plus' 'a' 'b') 'c' == 'plus' 'a' ('plus' 'b' 'c') @
-- [Identity of Multiplication] @'times' 'a' 'unit' == 'a' == 'times' 'unit' 'a'@
-- [Associativity Of Multiplication] @'times' ('times' 'a' 'b') 'c' == 'times' 'a' ('times' 'b' 'c') @
-- [Annihilation by Additive Identity through Multiplication] @'times' 'nil' 'a' == 'nil' == 'times' 'a' 'nil'@
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
