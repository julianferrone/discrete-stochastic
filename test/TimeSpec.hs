module TimeSpec (spec) where

import Test.Hspec
import Discrete.Stochastic.Time

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
  describe "Observable" $ do
    it "konst returns constant value at any time" $ do
      let o = konst 10
      sample o (Finite 1) `shouldBe` 10
      sample o Infinite `shouldBe` 10
    it "timeTo should return correct durations" $ do
      let o = timeTo $ Finite 10
      let t1 = Finite 2
      let t2 = Finite 15
      let t3 = Infinite
      sample o t1 `shouldBe` (Duration 8)
      sample o t2 `shouldBe` (Duration (-5))
      sample o t3 `shouldBe` Indeterminate
