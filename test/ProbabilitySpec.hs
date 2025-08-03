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
  describe "Dist" $ do
    it "accurately calculates the sum of two six-sided fair dice" $ do
      let d = combineOutcomes $ liftA2 (+) (die 6) (die 6)
      d
        `shouldBe` dist
          [ (2, prob (1 / 36)),
            (3, prob (2 / 36)),
            (4, prob (3 / 36)),
            (5, prob (4 / 36)),
            (6, prob (5 / 36)),
            (7, prob (6 / 36)),
            (8, prob (5 / 36)),
            (9, prob (4 / 36)),
            (10, prob (3 / 36)),
            (11, prob (2 / 36)),
            (12, prob (1 / 36))
          ]