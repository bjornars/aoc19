{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day03 where

import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Parser
import RIO
import RIO.List.Partial (foldl1, last, minimum)
import qualified RIO.Map as M
import qualified RIO.State as ST
import Util (distance)

data Dir = L | R | U | D deriving (Eq, Show, Ord)

data Turn = Turn Dir Int deriving (Eq, Show, Ord)

type Wire = [Turn]

type Point = (Int, Int)

type WireMap = M.Map Point Int

parseDir :: ReadP Dir
parseDir = char 'L' $> L <|> char 'R' $> R <|> char 'U' $> U <|> char 'D' $> D

parsePart :: ReadP Turn
parsePart = Turn <$> parseDir <*> digit

parseWire :: ReadP Wire
parseWire = sepBy1 parsePart (char ',')

parseWires :: ReadP [Wire]
parseWires = endBy parseWire (char '\n')

parseInput :: Text -> Maybe [Wire]
parseInput = parse (parseWires <* eof)

getInput :: IO [Wire]
getInput = readFileUtf8 "data/3" <&> (parseInput >>> fromJust)

getNext :: Int -> Point -> Turn -> [(Int, Point)]
getNext dist (startX, startY) = \case
  Turn _ 0 -> error "cannot deal with empty turns"
  Turn L num -> opX (-) <$> [1 .. num]
  Turn D num -> opY (-) <$> [1 .. num]
  Turn R num -> opX (+) <$> [1 .. num]
  Turn U num -> opY (+) <$> [1 .. num]
  where
    opX op p = (dist + p, (startX `op` p, startY))
    opY op p = (dist + p, (startX, startY `op` p))

makeWireMap :: Wire -> WireMap
makeWireMap = third . flip ST.execState (0, (0, 0), M.empty) . traverse step
  where
    third (_, _, a) = a
    step :: Turn -> ST.State (Int, Point, WireMap) ()
    step turn = do
      (dist, current, grid) <- ST.get
      let next = getNext dist current turn
      let end = last next
      let nextGrid = grid `M.union` M.fromList (swap <$> next)
      ST.put (fst end, snd end, nextGrid)

getIntersections :: [Wire] -> [(Point, [Int])]
getIntersections = fmap (makeWireMap >>> fmap (:[])) >>> foldl1 (M.intersectionWith (<>)) >>> M.toList

getClosestDistance :: [(Point, [Int])] -> Int
getClosestDistance = fmap (fst >>> distance (0, 0)) >>> minimum

getClosestLength :: [(Point, [Int])] -> Int
getClosestLength = fmap (snd >>> sum) >>> minimum

calc :: [Wire] -> Int
calc = getClosestDistance . getIntersections

calcLength :: [Wire] -> Int
calcLength = getClosestLength . getIntersections

part1 :: IO Int
part1 = calc <$> getInput

part2 :: IO Int
part2 = calcLength <$> getInput
