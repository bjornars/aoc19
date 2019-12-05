{-# LANGUAGE NoImplicitPrelude #-}
module Day05Spec (spec) where

import Import
import Day05
import Test.Hspec

spec :: Spec
spec = do
  describe "day05" $ do
    it "decodeIns" $ do
      decodeIns 10199 `shouldBe` (99, True, False, True)
      decodeIns 1102 `shouldBe` (02, True, True, False)

    it "part1" $ do
      res <- part1
      res `shouldBe` 9219874

    it "part2" $ do
      res <- part2
      res `shouldBe` 5893654
