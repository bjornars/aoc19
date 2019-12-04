{-# LANGUAGE NoImplicitPrelude #-}

module Day04Spec
  ( spec,
  )
where

import Day04
import Import
import Test.Hspec

spec :: Spec
spec = do
  describe "day04" $ do
    it "parseBounds parses" $ do
      parseBounds "123456-234567" `shouldBe` Just (123456, 234567)
      parseBounds "a23456-234567" `shouldBe` Nothing
      parseBounds "123456234567" `shouldBe` Nothing

    it "todDigit" $ do
      toDigit [] `shouldBe` 0
      toDigit [1,2,3] `shouldBe` 123

    it "part1" $ do
      part1 `shouldBe` 1764

    it "part2" $ do
      part2 `shouldBe` 1196
