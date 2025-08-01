module TimeSpec (spec) where

import Discrete.Stochastic.Semiring
import Discrete.Stochastic.Time
import Test.Hspec hiding (after)

spec :: Spec
spec = do
  describe "Time semiring laws hold" $ do
    it "Time `plus` nil == time == nil `plus` Time" $ do
      let t = Finite 2
      (t `plus` nil) `shouldBe` t
      (nil `plus` t) `shouldBe` t
    it "Time `times` unit == time == unit `times` Time" $ do
      let t = Finite 3
      (t `times` unit) `shouldBe` t
      (unit `times` t) `shouldBe` t
    it "Time `times` nil == nil" $ do
      let a = Finite 10
      a `times` nil `shouldBe` nil
      nil `times` a `shouldBe` nil
    it "Multiplication distributes over addition" $ do
      let a = Finite 4
      let b = Finite 5
      let c = Finite 6
      a `times` (b `plus` c) `shouldBe` (a `times` b) `plus` (a `times` c)
      (b `plus` c) `times` a `shouldBe` (b `times` a) `plus` (c `times` a)
  describe "Duration monoid laws hold" $ do
    it "Mappending two durations adds them together" $ do
      Duration 2 <> Duration 7 `shouldBe` Duration 9
    it "Mappending two durations is commutative" $ do
      Duration 5 <> Duration 10 `shouldBe` Duration 10 <> Duration 5
    it "Mappending a duration with identity is equal to the original identity" $ do
      Duration 1 <> mempty `shouldBe` Duration 1
      mempty <> Duration 1 `shouldBe` Duration 1
  describe "Time" $ do
    it "Previous returns Just Time for finite times after the epoch" $ do
      previous (Finite 5) `shouldBe` Just (Finite 4)
    it "Previous returns Nothing for the epoch" $ do
      previous epoch `shouldBe` Nothing
    it "Previous returns Nothing for the infinite future" $ do
      previous Infinite `shouldBe` Nothing
    it "Next returns the next value for a finite time" $ do
      next epoch `shouldBe` Finite 1
    it "Next returns Infinite for Infinite" $ do
      next Infinite `shouldBe` Infinite
  describe "Time and Duration interactions" $ do
    it "A Finite should be the same Finite after a mempty duration has passed" $ do
      mempty `after` Finite 5 `shouldBe` Finite 5
    it "An Infinite should be Infinite after any duration has passed" $ do
      Duration 5 `after` Infinite `shouldBe` Infinite
    it "Two Finite times have a determinate Duration between them" $ do
      Finite 3 `to` Finite 10 `shouldBe` Duration 7
      Finite 10 `to` Finite 8 `shouldBe` Duration (-2)
    it "An Infinite time has an Indeterminate duration to/from any other time" $ do
      Infinite `to` Finite 1 `shouldBe` Indeterminate
      Finite 2 `to` Infinite `shouldBe` Indeterminate
      Infinite `to` Infinite `shouldBe` Indeterminate
  describe "Observable" $ do
    it "konst returns constant value at any time" $ do
      let obs = konst "abc"
      sample obs (Finite 1) `shouldBe` "abc"
      sample obs Infinite `shouldBe` "abc"
    it "timeTo should return correct durations" $ do
      let obs = timeTo $ Finite 10
      let t1 = Finite 2
      let t2 = Finite 15
      let t3 = Infinite
      sample obs t1 `shouldBe` Duration 8
      sample obs t2 `shouldBe` Duration (-5)
      sample obs t3 `shouldBe` Indeterminate
  describe "Observable functor laws hold" $ do
    it "fmap id obs == obs" $ do
      let obs = timeTo $ Finite 10
      let t = Finite 5
      sample obs t `shouldBe` sample (fmap id obs) t
    it "fmap f (fmap g obs) == fmap (f . g) obs" $ do
      let obs = konst 0
      let f = show
      let g = (+ 1)
      let t = Finite 10
      sample (fmap f (fmap g obs)) t `shouldBe` sample (fmap (f . g) obs) t
  describe "Duration semiring laws hold" $ do
    it "Duration `plus` nil == time == nil `plus` Duration" $ do
      let d = Duration 2
      (d `plus` nil) `shouldBe` d
      (nil `plus` d) `shouldBe` d
    it "Indeterminate `plus` nil == time == nil `plus` Indeterminate" $ do
      let d = Indeterminate
      (d `plus` nil) `shouldBe` d
      (nil `plus` d) `shouldBe` d
    it "Duration `times` unit == time == unit `times` Duration" $ do
      let d = Duration 3
      (d `times` unit) `shouldBe` d
      (unit `times` d) `shouldBe` d
    it "Indeterminate `times` unit == time == unit `times` Indeterminate" $ do
      let d = Indeterminate
      (d `times` unit) `shouldBe` d
      (unit `times` d) `shouldBe` d
    it "Duration `times` nil == nil" $ do
      let a = Duration 10
      a `times` nil `shouldBe` nil
      nil `times` a `shouldBe` nil
    it "Indeterminate `times` nil == nil" $ do
      let a = Indeterminate
      a `times` nil `shouldBe` nil
      nil `times` a `shouldBe` nil
    it "Multiplication distributes over addition" $ do
      let a = Duration 4
      let b = Duration 5
      let c = Duration 6
      a `times` (b `plus` c) `shouldBe` (a `times` b) `plus` (a `times` c)
      (b `plus` c) `times` a `shouldBe` (b `times` a) `plus` (c `times` a)
  describe "Observable semiring" $ do
    it "adding two Observables together adds their underlying values pointwise" $ do
      let obs1 = timeTo (Finite 3)
      let obs2 = timeTo (Finite 5)
      sample (obs1 `plus` obs2) epoch `shouldBe` Duration 5
    it "multiplying two Observables together multiplies their underlying values pointwise" $ do
      let obs1 = timeTo (Finite 1)
      let obs2 = timeTo (Finite 2)
      sample (obs1 `times` obs2) epoch `shouldBe` Duration 1