{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day13Spec
  ( spec,
  )
where

import Day13
import Import
import Test.Hspec

spec :: Spec
spec = do
  describe "day13" $ do
    it "part1" $ do
      res <- part1
      res `shouldBe` 173
    it "part2" $ do
      res <- part2
      res `shouldBe` 8942
