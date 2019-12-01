{-# LANGUAGE NoImplicitPrelude #-}

module Day01 where

import RIO
import RIO.List (iterate)
import qualified RIO.Text as T

import Util (asFloat, readInt)

calc :: Integer -> Integer
calc = asFloat >>> (/3) >>> floor >>> (subtract 2)

calcWithFuel :: Integer -> Integer
calcWithFuel = sum . drop 1 . takeWhile (>0) . iterate calc

getInput :: IO [Integer]
getInput = do
  input <- T.lines <$> readFileUtf8 "data/1"
  return . fold $ traverse readInt input

part1 :: IO Integer
part1 = do
  input <- getInput
  return . sum . fmap calc $ input

part2 :: IO Integer
part2 = do
  input <- getInput
  return . sum . fmap calcWithFuel $ input
