{-# LANGUAGE NoImplicitPrelude #-}
module Day07Spec (spec) where

import Import
import Day07
import Test.Hspec

spec :: Spec
spec = do
  describe "day07" $ do
    it "example1" $ do
      let program = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
      runInSeries program 0 [4,3,2,1,0] `shouldBe` 43210
      findBest program [0..4] `shouldBe` 43210

    it "example2" $ do
      let program = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26, 27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
      runInFeedback program [9,8,7,6,5] `shouldBe` 139629729

    it "part1" $ do
      res <- part1
      res `shouldBe` 45730

    it "part2" $ do
      res <- part2
      res `shouldBe` 5406484
