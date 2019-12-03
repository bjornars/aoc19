{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day03 where

import RIO
import qualified RIO.State as ST
import qualified RIO.Set as S

import RIO.List.Partial (minimum, foldl1, last)
import Data.Maybe (fromJust)

import Parser
import Util (distance)

data Dir = L | R | U | D deriving (Eq, Show, Ord)
data Turn = Turn Dir Int deriving (Eq, Show, Ord)
type Wire = [Turn]

type Point = (Int, Int)
type WireMap = Set Point

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

getNext :: Point -> Turn -> [Point]
getNext _ (Turn _ 0) = error "cannot deal with empty turns"
getNext coords (Turn L num) = flip (first . flip (-)) coords <$> [1..num]
getNext coords (Turn D num) = flip (second . flip (-)) coords <$> [1..num]
getNext coords (Turn R num) = flip (first . (+)) coords <$> [1..num]
getNext coords (Turn U num) = flip (second . (+)) coords <$> [1..num]

makeWireMap :: Wire -> WireMap
makeWireMap = snd . flip ST.execState ((0, 0), S.empty) . traverse step
  where step :: Turn -> ST.State (Point , WireMap) ()
        step turn = do
          (current, grid) <- ST.get
          let next = getNext current turn
          ST.put (last next, foldr S.insert grid next)

getIntersections :: [Wire] -> [Point]
getIntersections = fmap makeWireMap >>> foldl1 S.intersection >>> toList

getClosestDistance :: [Point] -> Int
getClosestDistance = fmap (distance (0, 0)) >>> minimum

calc :: [Wire] -> Int
calc = getClosestDistance . getIntersections

part1 :: IO Int
part1 = calc <$> getInput

part2 :: IO Int
part2 = do
  return 0
