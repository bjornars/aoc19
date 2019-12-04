{-# LANGUAGE NoImplicitPrelude #-}

module Day04 where

import Data.Maybe (fromJust)
import RIO
import RIO.List (group)

toDigit :: [Int] -> Int
toDigit = go . reverse
  where
    go (x : xs) = x + 10 * go xs
    go [] = 0

parseBounds :: String -> Maybe (Int, Int)
parseBounds (x1 : x2 : x3 : x4 : x5 : x6 : '-' : y1 : y2 : y3 : y4 : y5 : y6 : []) =
  (,) <$> (readMaybe [x1, x2, x3, x4, x5, x6])
      <*> (readMaybe [y1, y2, y3, y4, y5, y6])
parseBounds _ = Nothing

hasOneDuplicate :: Eq a => [a] -> Bool
hasOneDuplicate = group >>> any (length >>> (>= 2))

hasATwoRun :: Eq a => [a] -> Bool
hasATwoRun = group >>> any (length >>> (== 2))

calc :: ([Int] -> Bool) -> (Int, Int) -> [Int]
calc pred (lowerBound, upperBound) = do
  d1 <- [0 .. 6]
  d2 <- [d1 .. 9]
  d3 <- [d2 .. 9]
  d4 <- [d3 .. 9]
  d5 <- [d4 .. 9]
  d6 <- [d5 .. 9]
  let digits = [d1, d2, d3, d4, d5, d6]
  let res = toDigit digits
  guard $ res >= lowerBound && res <= upperBound
  guard $ pred digits
  return res

bounds :: (Int, Int)
bounds = fromJust $ parseBounds "152085-670283"

part1 :: Int
part1 = length $ calc hasOneDuplicate bounds

part2 :: Int
part2 = length $ calc hasATwoRun bounds
