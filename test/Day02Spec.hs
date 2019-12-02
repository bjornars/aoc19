{-# LANGUAGE NoImplicitPrelude #-}
module Day02Spec (spec) where

import Import
import Day02
import Test.Hspec

spec :: Spec
spec = do
  describe "day02" $ do
    it "exec" $ do
      let exec' arr = exec arr (\_ -> return ())
      exec' [1,0,0,0,99] `shouldBe` [2, 0, 0, 0, 99]
      exec' [2,3,0,3,99] `shouldBe` [2,3,0,6,99]
      exec' [2,4,4,5,99,0] `shouldBe` [2,4,4,5,99,9801]
      exec' [1,1,1,4,99,5,6,0,99] `shouldBe` [30,1,1,4,2,5,6,0,99]

    it "part1" $ do
      res <- part1
      res `shouldBe` 5434663


    it "part2" $ do
      res <- part2
      res `shouldBe` 4559
