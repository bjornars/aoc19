{-# LANGUAGE NoImplicitPrelude #-}
module Day01Spec (spec) where

import Import
import Day01
import Test.Hspec

spec :: Spec
spec = do
  describe "day01" $ do
    it "calc" $ do
      calc 12 `shouldBe` 2
      calc 14 `shouldBe` 2
      calc 1969 `shouldBe` 654
      calc 100756 `shouldBe` 33583

    it "calcWithFuel" $ do
      calcWithFuel 1969 `shouldBe` 966
      calcWithFuel 100756 `shouldBe` 50346

    it "part 1 " $ do
      part1 >>= (`shouldBe` 3305301)

    it "part 2 " $ do
      part2 >>= (`shouldBe` 4955106)
