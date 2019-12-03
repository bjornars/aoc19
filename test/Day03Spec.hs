{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day03Spec
  ( spec,
  )
where

import Day03
import Import
import Parser
import qualified RIO.Map as M
import Test.Hspec

parse' :: ReadP a -> Text -> Maybe a
parse' p = parse (p <* eof)

spec :: Spec
spec = do
  describe "day03" $ do
    it "parser parses" $ do
      parse' parsePart "R53" `shouldBe` Just (Turn R 53)
      parseInput "R1002,D715,R356,D749\nL998,U258,R975\n"
        `shouldBe` Just
          [ [Turn R 1002, Turn D 715, Turn R 356, Turn D 749],
            [Turn L 998, Turn U 258, Turn R 975]
          ]
    it "makes wireMaps" $ do
      let wiremap = makeWireMap [Turn R 5, Turn U 3, Turn R 2, Turn D 1]
      wiremap
        `shouldBe` M.fromList
          [ ((1, 0), 1),
            ((2, 0), 2),
            ((3, 0), 3),
            ((4, 0), 4),
            ((5, 0), 5),
            ((5, 1), 6),
            ((5, 2), 7),
            ((5, 3), 8),
            ((6, 3), 9),
            ((7, 3), 10),
            ((7, 2), 11)
          ]
    it "calculates intersections" $ do
      let wires = parseInput "R8,U5,L5,D3\nU7,R6,D4,L4\n"
      getIntersections <$> wires `shouldBe` Just [((3, 3), [20, 20]), ((6, 5), [15, 15])]
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

    it "example2-2" $ do
      let wires = parseInput "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83\n"
      calcLength <$> wires `shouldBe` Just 610
    it "example3-2" $ do
      let wires = parseInput "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7\n"
      calcLength <$> wires `shouldBe` Just 410

    it "part2" $ do
      res <- part2
      res `shouldBe` 48054
