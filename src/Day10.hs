{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day10 where

import Data.Foldable (maximum, maximumBy, minimumBy)
import Data.List (iterate, sortBy)
import Data.List.NonEmpty (groupWith)
import RIO
import RIO.List.Partial ((!!))
import qualified RIO.Set as S
import qualified RIO.Text as T

type Point = (Int, Int)

getBounds :: (Foldable t, Ord a) => t (a, a) -> (a, a)
getBounds f = let f' = toList f in (maximum (fst <$> f'), maximum (snd <$> f'))

parse :: Text -> (Point, Set Point)
parse text =
  let squares = S.fromList $
        do
          (y, line) <- zip [0 ..] $ T.lines text
          (x, char) <- zip [0 ..] $ T.unpack line
          guard $ char /= '.'
          pure (x, y)
   in (getBounds squares, squares)

getInput :: IO (Point, Set Point)
getInput = parse <$> readFileUtf8 "data/10"

both :: (t1 -> t2 -> b) -> (t1, t1) -> (t2, t2) -> (b, b)
both op (a, b) (c, d) = (a `op` c, b `op` d)

padd :: (Int, Int) -> (Int, Int) -> (Int, Int)
padd = both (+)

psub :: (Int, Int) -> (Int, Int) -> (Int, Int)
psub = both (-)

inBounds :: Point -> Point -> Bool
inBounds _ (x, _) | x < 0 = False
inBounds _ (_, y) | y < 0 = False
inBounds (mx, _) (x, _) | x > mx = False
inBounds (_, my) (_, y) | y > my = False
inBounds _ _ | otherwise = True

vec2deg :: Point -> Float
vec2deg (x, y) = 180 * atan2 (fromIntegral x) (fromIntegral (- y)) / pi

simplify :: Point -> Point
simplify (a, b) =
  let g = gcd a b
   in if
        | g == 1 -> (a, b)
        | g == 0 -> (0, 0)
        | otherwise -> (a `div` g, b `div` g)

rays :: Point -> Point -> Point -> [Point]
rays bounds source point =
  let delta = simplify $ point `psub` source
   in takeWhile (inBounds bounds) . drop 1 $ iterate (padd delta) point

distance :: (Ord a, Num a) => (a, a) -> (a, a) -> a
distance (x1, y1) (x2, y2) = abs (x1 - x2) `max` abs (y1 - y2)

findVisibles :: Point -> Set Point -> Point -> Int
findVisibles bounds candiates source =
  let can' = source `S.delete` candiates
   in go can' 1
  where
    go xs _ | S.null xs = 0
    go xs n =
      let (visibles, next) = S.partition (\x -> distance x source == n) xs
          obscured' = concatMap (rays bounds source) $ toList visibles
          obscured = next `S.intersection` S.fromList obscured'
          toCheck = next `S.difference` obscured
       in length visibles + go toCheck (n + 1)

findBest :: Point -> Set Point -> (Point, Int)
findBest bounds candiates =
  let res = (id &&& findVisibles bounds candiates) <$> (toList candiates)
   in maximumBy (compare `on` snd) res

part1 :: IO Int
part1 = do
  (bounds, field) <- getInput
  pure . snd $ findBest bounds field

shoot :: Point -> S.Set Point -> [Point]
shoot _ xs |S.null xs = []
shoot src xs =
  let angle point = let a = vec2deg (point `psub` src) in if a < 0 then a + 360 else a
      targets = (id &&& angle) <$> toList xs
      eqClasses = groupWith snd $ sortBy (compare `on` snd) targets
      closestPerClass = minimumBy (compare `on` (distance src . fst)) <$> eqClasses
      hits = fmap fst closestPerClass
   in hits <> shoot src (foldl' (flip S.delete) xs hits)

part2 :: IO Int
part2 = do
  (bounds, field) <- getInput
  let target = fst $ findBest bounds field
  let (x, y) = shoot target (S.delete target field) !! 199
  pure $ x * 100 + y
