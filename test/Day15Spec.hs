{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day15Spec
  ( spec,
  )
where

import Day15
import Import
import Test.Hspec

spec :: Spec
spec = do
  describe "day15" $ do
    it "part1" $ do
      res <- part1
      res `shouldBe` 254
    it "part2" $ do
      res <- part2
      res `shouldBe` 268
