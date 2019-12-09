{-# LANGUAGE NoImplicitPrelude #-}

module Day09Spec
  ( spec,
  )
where

import Day09
import Import
import Test.Hspec

spec :: Spec
spec = do
  describe "day09" $ do
    it "example1" $ do
      let program =
            [ 109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99]
      exec [] program `shouldBe` program
    it "example2" $ do
      let program = [1102, 34915192, 34915192, 7, 4, 7, 99, 0]
      exec [] program `shouldBe` [1219070632396864]
    it "example3" $ do
      let program = [104, 1125899906842624, 99]
      exec [] program `shouldBe` [1125899906842624]
    it "part1" $ do
      res <- part1
      res `shouldBe` 2662308295
    it "part2" $ do
      res <- part2
      res `shouldBe` 63441
