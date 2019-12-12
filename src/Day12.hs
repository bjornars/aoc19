{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day12 where

import Data.Maybe (fromJust)
import Data.Monoid (Sum (..), getSum)
import qualified Parser as P
import RIO
import RIO.List (findIndex, iterate)
import RIO.List.Partial (tail, (!!))

data Vec3 a = Vec3 {x :: a, y :: a, z :: a} deriving (Show, Eq)

zero3 :: Vec3 Int
zero3 = Vec3 0 0 0

vop :: (a -> a -> a) -> Vec3 a -> Vec3 a -> Vec3 a
vop f (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
  Vec3
    (x1 `f` x2)
    (y1 `f` y2)
    (z1 `f` z2)

vplus :: Vec3 Int -> Vec3 Int -> Vec3 Int
vplus = vop (+)

vsum :: Foldable f => f (Vec3 Int) -> Vec3 Int
vsum = foldl' vplus zero3

vtotal :: Vec3 Int -> Int
vtotal (Vec3 x1 y1 z1) = abs x1 + abs y1 + abs z1

energy :: Moon -> Int
energy (Moon vel pos) = vtotal vel * vtotal pos

data Moon = Moon {velocity :: Vec3 Int, position :: Vec3 Int} deriving (Show, Eq)

data Jupiter = Jupiter [Moon] deriving (Show, Eq)

moon :: P.ReadP Moon
moon =
  (\x' y' z' -> Moon zero3 (Vec3 x' y' z'))
    <$> (P.char '<' *> p 'x' <* P.string ", ")
    <*> (p 'y' <* P.string ", ")
    <*> (p 'z' <* P.char '>')
  where
    p c = P.char c *> P.char '=' *> P.digit

parser :: P.ReadP Jupiter
parser = Jupiter <$> P.endBy moon (P.char '\n')

getInput :: IO (Maybe Jupiter)
getInput = P.parse (parser <* P.eof) <$> readFileUtf8 "data/12"

getPairwise :: [a] -> [a] -> [(a, [a])]
getPairwise [] _ = []
getPairwise (m : ms) done = (m, ms <> done) : getPairwise ms (m : done)

step' :: [Moon] -> [Moon]
step' moons =
  let combinations = getPairwise moons []
      math :: (Moon, [Moon]) -> Moon
      math (this, other) =
        let computeDeltaV v1 v2 = case v1 `compare` v2 of
              LT -> 1
              EQ -> 0
              GT -> -1
            deltaV = foldl' vplus zero3 [vop computeDeltaV (position this) (position t) | t <- other]
            newV = velocity this `vplus` deltaV
            newPos = position this `vplus` newV
         in Moon newV newPos
      m1 = math <$> combinations
   in m1

step :: Jupiter -> Jupiter
step (Jupiter moons) = Jupiter (step' moons)

part1 :: IO Int
part1 = do
  system <- fromJust <$> getInput
  let iter = iterate step system
  let Jupiter moons = iter !! 1000
  pure . getSum $ foldMap (energy >>> Sum) moons

xSystem :: Jupiter -> [Int]
xSystem (Jupiter moons) = concatMap (\(Moon (Vec3 x1 _ _) (Vec3 x2 _ _)) -> [x1, x2]) moons
ySystem :: Jupiter -> [Int]
ySystem (Jupiter moons) = concatMap (\(Moon (Vec3 _ y1 _) (Vec3 _ y2 _)) -> [y1, y2]) moons
zSystem :: Jupiter -> [Int]
zSystem (Jupiter moons) = concatMap (\(Moon (Vec3 _ _ z1) (Vec3 _ _ z2)) -> [z1, z2]) moons

findCycle :: Jupiter -> Integer
findCycle system = let
  -- find the cycle length per axis, and return the lcm
  iter = tail $ iterate step system
  initX = xSystem $ system
  initY = ySystem $ system
  initZ = zSystem $ system
  find s = (+1) . fromIntegral . fromJust . findIndex (==s)
  xlen = find initX $ xSystem <$> iter
  ylen = find initY $ ySystem <$> iter
  zlen = find initZ $ zSystem <$> iter
  in xlen `lcm` ylen `lcm` zlen

part2 :: IO Integer
part2 = do
  system <- fromJust <$> getInput
  pure $ findCycle system
