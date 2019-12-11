{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day11Spec
  ( spec,
  )
where

import Day11
import Import
import Test.Hspec

spec :: Spec
spec = do
  describe "day11" $ do
    it "part1" $ do
      res <- part1
      res `shouldBe` 2319
    it "part2" $ do
      res <- part2
      res `shouldBe` "UERPRFGJ"
