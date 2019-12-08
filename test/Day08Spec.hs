{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day08Spec
  ( spec,
  )
where

import Day08
import Import
import RIO.Partial
import Test.Hspec

spec :: Spec
spec = do
  describe "day08" $ do
    it "part1" $ do
      res <- part1
      res `shouldBe` 2159

    it "pixel combines" $ do
      foldMap toEnum [] `shouldBe` Transparent
      foldMap toEnum [0, 1, 2, 0] `shouldBe` Black
      foldMap toEnum [2, 1, 2, 0] `shouldBe` White
      foldMap toEnum [2, 2, 1, 0] `shouldBe` White
      foldMap toEnum [2, 2, 2, 0] `shouldBe` Black

    it "groupByPixel" $ do
      let image = [[[0, 2], [2, 2]], [[1, 1], [2, 2]],
                   [[2, 2], [1, 2]], [[0, 0], [0, 0]]]
      groupByPixel image
        `shouldBe` [ [[0, 1, 2, 0], [2, 1, 2, 0]],
                     [[2, 2, 1, 0], [2, 2, 2, 0]]
                   ]

    it "part2" $ do
      res <- part2
      res `shouldBe` "\9608  \9608\9608\9608\9608  \9608    \9608 \9608\9608 \9608   \9608\9608\n \9608\9608 \9608\9608\9608\9608 \9608\9608\9608\9608 \9608 \9608\9608 \9608 \9608\9608 \9608\n \9608\9608\9608\9608\9608\9608\9608 \9608\9608\9608 \9608\9608    \9608 \9608\9608 \9608\n \9608\9608\9608\9608\9608\9608\9608 \9608\9608 \9608\9608\9608 \9608\9608 \9608   \9608\9608\n \9608\9608 \9608 \9608\9608 \9608 \9608\9608\9608\9608 \9608\9608 \9608 \9608 \9608\9608\n\9608  \9608\9608\9608  \9608\9608    \9608 \9608\9608 \9608 \9608\9608 \9608"
