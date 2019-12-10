{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day10Spec
  ( spec,
  )
where

import Day10
import Import
import RIO.List (splitAt)
import RIO.List.Partial ((!!))
import qualified RIO.Set as S
import System.IO
import Test.Hspec

input1, input2, input3 :: Text
input1 = ".#..#\n.....\n#####\n....#\n...##\n"
input2 = "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####\n"
input3 = "#.........\n...A......\n...B..a...\n.EDCG....a\n..F.c.b...\n.....c....\n..efd.c.gb\n.......c..\n....f...c.\n...e..d..c\n"

printStuff :: Set Point -> [Point] -> Point -> IO ()
printStuff _ [] _ = pure ()
printStuff field xs (mx, my) = do
  let (stuff, rest) = splitAt 9 xs
  let m = zip stuff [1 :: Integer ..]
  forM_ [0 .. my] $ \y -> do
    forM_ [0 .. mx] $ \x ->
      case lookup (x, y) m of
        Just n -> putStr $ show n
        Nothing -> putStr $ if S.member (x, y) field then "#" else "."
    putStrLn ""
  putStrLn ""
  printStuff field rest (mx, my)

spec :: Spec
spec = do
  describe "day10" $ do
    it "getInput" $ do
      (inputBounds, asteroids) <- getInput
      inputBounds `shouldBe` (30, 30)
      -- (x, y), not (y, x)
      S.member (0, 30) asteroids `shouldBe` True
      S.member (30, 0) asteroids `shouldBe` False

    it "example1" $ do
      let (bounds, field) = parse input1
      findVisibles bounds field (3, 4) `shouldBe` 8
      findBest bounds field `shouldBe` ((3, 4), 8)

    it "simplify" $ do
      simplify (0, 6) `shouldBe` (0, 1)
      simplify (6, 0) `shouldBe` (1, 0)
      simplify (0, -6) `shouldBe` (0, -1)
      simplify (-6, 0) `shouldBe` (-1, 0)
      simplify (6, 3) `shouldBe` (2, 1)
      simplify (4, 3) `shouldBe` (4, 3)
      simplify (4, 8) `shouldBe` (1, 2)
      simplify (-4, -8) `shouldBe` (-1, -2)

    it "example2" $ do
      let (bounds, field) = parse input2
      findVisibles bounds field (5, 8) `shouldBe` 33
      findBest bounds field `shouldBe` ((5, 8), 33)

    it "example3" $ do
      let (bounds, field) = parse input3
      findVisibles bounds field (0, 0) `shouldBe` 7

    it "part1" $ do
      res <- part1
      res `shouldBe` 288

    it "vec2deg" $ do
      vec2deg (0, -1) `shouldBe` 0
      vec2deg (2, 2) `shouldBe` 135

    it "example1 pt 2" $ do
      let i = ".#....#####...#..\n##...##.#####..##\n##...#...#.#####.\n..#.....X...###..\n..#.#.....#....##\n"
      let (bounds, field) = parse i
      let target = fst $ findBest bounds field
      let res = shoot target (S.delete target field)
      -- printStuff field res bounds
      res `shouldBe` [(8, 1), (9, 0), (9, 1), (10, 0), (9, 2), (11, 1), (12, 1), (11, 2), (15, 1), (12, 2), (13, 2), (14, 2), (15, 2), (12, 3), (16, 4), (15, 4), (10, 4), (4, 4), (2, 4), (2, 3), (0, 2), (1, 2), (0, 1), (1, 1), (5, 2), (1, 0), (5, 1), (6, 1), (6, 0), (7, 0), (8, 0), (10, 1), (14, 0), (16, 1), (13, 3), (14, 3)]

    it "example2 pt 2" $ do
      let i = ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##\n"
      let (bounds, field) = parse i
      let target = fst $ findBest bounds field
      target `shouldBe` (11, 13)
      let res = shoot target (S.delete target field)
      let picks = (res !!) <$> [0, 1, 2, 9, 19, 49, 99, 198, 199, 200, 298]
      picks `shouldBe` [(11, 12), (12, 1), (12, 2), (12, 8), (16, 0), (16, 9), (10, 16), (9, 6), (8, 2), (10, 9), (11, 1)]

    it "part2" $ do
      res <- part2
      res `shouldBe` 616
