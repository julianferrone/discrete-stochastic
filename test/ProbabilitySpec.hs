module ProbabilitySpec (spec) where

import Discrete.Stochastic.Probability
import Test.Hspec

spec :: Spec
spec = do
  describe "Prob" $ do
    it "pnot almostSurely == almostNever" $ do
      pnot almostSurely `shouldBe` almostNever
    it "pnot almostNever == almostSurely" $ do
      pnot almostNever `shouldBe` almostSurely