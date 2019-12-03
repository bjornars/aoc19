{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day03Spec
  ( spec
  ) where

import Day03
import Import
import Parser
import Test.Hspec

import qualified RIO.Set as S

parse' :: ReadP a -> Text -> Maybe a
parse' p = parse (p <* eof)

spec :: Spec
spec = do
  describe "day03" $ do
    it "parser parses" $ do
      parse' parsePart "R53" `shouldBe` Just (Turn R 53)
      parseInput "R1002,D715,R356,D749\nL998,U258,R975\n" `shouldBe`
        Just
          [ [Turn R 1002, Turn D 715, Turn R 356, Turn D 749]
          , [Turn L 998, Turn U 258, Turn R 975]
          ]

    it "makes wireMaps" $ do
      let wiremap = makeWireMap [Turn R 5, Turn U 3, Turn R 2, Turn D 1]
      wiremap `shouldBe` S.fromList [(1,0),(2,0),(3,0),(4,0),(5,0),(5,1),(5,2),(5,3),(6,3),(7,3),(7,2)]

    it "calculates intersections" $ do
      let wires = parseInput "R8,U5,L5,D3\nU7,R6,D4,L4\n"
      getIntersections <$> wires `shouldBe` Just [(3, 3), (6, 5)]

    it "example1" $ do
      let wires = parseInput "R8,U5,L5,D3\nU7,R6,D4,L4\n"
      calc <$> wires `shouldBe` Just 6

    it "example2" $ do
      let wires = parseInput "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83\n"
      calc <$> wires `shouldBe` Just 159

    it "example3" $ do
      let wires = parseInput "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7\n"
      calc <$> wires `shouldBe` Just 135

    it "part1" $ do
      res <- part1
      res `shouldBe` 3247
