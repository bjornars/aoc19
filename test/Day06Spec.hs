{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Day06Spec (spec) where

import Import
import Day06
import Test.Hspec

testData :: Text
testData = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\n"

transferData :: Text
transferData = testData <> "K)YOU\nI)SAN\n"

spec :: Spec
spec = do
  describe "day05" $ do
    it "testData" $ do
     let t = parseData testData
     length (pathToCom t "D") `shouldBe` 3
     length (pathToCom t "L") `shouldBe` 7
     calcOrbits t `shouldBe` 42

    it "transferData" $ do
     let t = parseData transferData
     let you = pathToCom t "YOU"
     let san = pathToCom t "SAN"
     you`shouldBe` ["YOU","K","J","E","D","C","B"]
     san `shouldBe` ["SAN","I","D","C","B"]
     findCommonSuffix you san `shouldBe` ["B", "C", "D"]
     transfers t `shouldBe` 4

    it "part1" $ do
      res <- part1
      res `shouldBe` 453028

    it "part2" $ do
      res <- part2
      res `shouldBe` 562
