{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day08 where

import Data.Foldable (minimumBy)

import RIO
import RIO.List (transpose)
import RIO.Partial (toEnum)
import qualified RIO.Text as T
import System.IO (putStrLn)
import Util (chunk)

type Row = [Integer]
type Layer = [Row]
type Image = [Layer]

data Pixel = Black | White | Transparent deriving (Eq, Enum)

instance Semigroup Pixel where
  Black <> _ = Black
  White <> _ = White
  Transparent <> x = x

instance Monoid Pixel where
  mempty = Transparent

instance Show Pixel where
  show Black = "█"
  show White = " "
  show Transparent = "░"

readSingleInt :: Char -> Maybe Integer
readSingleInt = (: []) >>> readMaybe

getInput :: IO Image
getInput = do
  input <- (T.unpack >>> fmap readSingleInt >>> catMaybes) <$> readFileUtf8 "data/8"
  let rows = chunk 25 input
  let layers = chunk 6 rows
  return $ layers

count :: Integer -> Layer -> Integer
count c = concat >>> filter (==c) >>> fmap (const 1) >>> sum

part1 :: IO Integer
part1 = do
  layers <- getInput
  let layerBy0 = fmap (count 0 &&& id) layers
  let targetLayer = minimumBy (compare `on` fst) layerBy0 & snd
  pure $ (count 1 targetLayer * count 2 targetLayer)

groupByPixel :: Image -> [[[Integer]]]
groupByPixel = fmap transpose . transpose

toPixel :: [Integer] -> Pixel
toPixel = foldMap (fromIntegral >>> toEnum)

renderPixel :: [Integer] -> Text
renderPixel = toPixel >>> show >>> T.pack

part2 :: IO Text
part2 = do
  layers <- getInput
  let pixels = (fmap.fmap) renderPixel $ groupByPixel layers
  let answer = T.intercalate "\n"  $ (fmap mconcat) pixels
  putStrLn . T.unpack $ answer
  pure $ answer
