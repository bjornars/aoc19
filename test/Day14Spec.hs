{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day14Spec
  ( spec,
  )
where

import Data.Maybe (fromJust)
import Day14
import Import
import qualified Parser as P
import Test.Hspec

input1 :: Text
input1 = "10 ORE => 10 A\n1 ORE => 1 B\n7 A, 1 B => 1 C\n7 A, 1 C => 1 D\n7 A, 1 D => 1 E\n7 A, 1 E => 1 FUEL\n"

input2 :: Text
input2 = "171 ORE => 8 CNZTR\n7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL\n114 ORE => 4 BHXH\n14 VRPVC => 6 BMBT\n6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL\n6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT\n15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW\n13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW\n5 BMBT => 4 WPTQ\n189 ORE => 9 KTJDG\n1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP\n12 VRPVC, 27 CNZTR => 2 XDBXC\n15 KTJDG, 12 BHXH => 5 XCVML\n3 BHXH, 2 VRPVC => 7 MZWV\n121 ORE => 7 VRPVC\n7 XCVML => 6 RJRHP\n5 BHXH, 4 VRPVC => 5 LTCX\n"

spec :: Spec
spec = do
  describe "day14" $ do
    it "parser" $ do
      P.parse (parseText <* P.eof) "ORE" `shouldBe` Just "ORE"
      P.parse (parseReagent <* P.eof) "15 ORE" `shouldBe` Just (Reagent 15 "ORE")
      P.parse (parseReaction <* P.eof) "15 ORE, 12 FOO => 2 FUEL"
        `shouldBe` Just
          ( Reaction
              [Reagent 15 "ORE", Reagent 12 "FOO"]
              (Reagent 2 "FUEL")
          )
      --input1 = "10 ORE => 10 A\n1 ORE => 1 B\n7 A, 1 B => 1 C\n7 A, 1 C => 1 D\n7 A, 1 D => 1 E\n7 A, 1 E => 1 FUEL\n"
      P.parse (parser <* P.eof) input1
        `shouldBe` Just
          [ Reaction
              [Reagent 10 "ORE"]
              (Reagent 10 "A"),
            Reaction
              [Reagent 1 "ORE"]
              (Reagent 1 "B"),
            Reaction
              [Reagent 7 "A", Reagent 1 "B"]
              (Reagent 1 "C"),
            Reaction
              [Reagent 7 "A", Reagent 1 "C"]
              (Reagent 1 "D"),
            Reaction
              [Reagent 7 "A", Reagent 1 "D"]
              (Reagent 1 "E"),
            Reaction
              [Reagent 7 "A", Reagent 1 "E"]
              (Reagent 1 "FUEL")
          ]
    it "getCycles" $ do
      getCycles 0 5 `shouldBe` (0, 0)
      getCycles 4 5 `shouldBe` (1, 1)
      getCycles 5 5 `shouldBe` (1, 0)
      getCycles 6 5 `shouldBe` (2, 4)
      getCycles 15 5 `shouldBe` (3, 0)
      getCycles 16 5 `shouldBe` (4, 4)
      getCycles 14 5 `shouldBe` (3, 1)
    it "example1" $ do
      let parsed = P.parse (parser <* P.eof) input1
      exec 1 (fromJust parsed) `shouldBe` 31
    it "example2" $ do
      let parsed = P.parse (parser <* P.eof) input2
      exec 1 (fromJust parsed) `shouldBe` 2210736
    it "part1" $ do
      res <- part1
      res `shouldBe` 1185296
    it "example2 part2" $ do
      let parsed = P.parse (parser <* P.eof) input2
      findN (fromJust parsed) 1000000000000 `shouldBe` 460664
    it "part2" $ do
      res <- part2
      res `shouldBe` 1376631
