{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day06 where

import Data.Monoid
import Data.Tuple
import RIO
import qualified RIO.Map as M
import qualified RIO.Text as T

partition :: Char -> Text -> Maybe (Text, Text)
partition delim str = case T.split (== delim) str of
  ([x, y]) -> Just (x, y)
  _ -> Nothing

type Orbits = Map Text Text

parseData :: Text -> Orbits
parseData =
  T.lines
    >>> traverse (partition ')')
    >>> fromMaybe []
    >>> fmap swap
    >>> M.fromList

pathToCom :: Orbits -> Text -> [Text]
pathToCom _ "COM" = []
pathToCom os str = case M.lookup str os of
  Just next -> str : pathToCom os next
  Nothing -> error . T.unpack $ "missing orbit for " <> str

calcOrbits :: Orbits -> Int
calcOrbits os =
  getSum . foldMap (Sum . length . pathToCom os) $ M.keys os

findCommonPrefix :: Eq a => [a] -> [a] -> [a]
findCommonPrefix (x : xs) (y : ys) | x == y = x : findCommonPrefix xs ys
findCommonPrefix _ _ = []

findCommonSuffix :: Eq a => [a] -> [a] -> [a]
findCommonSuffix = findCommonPrefix `on` reverse

transfers :: Orbits -> Int
transfers os =
  let you = pathToCom os "YOU"
      san = pathToCom os "SAN"
      shared = findCommonSuffix you san
   in length you + length san - (length shared * 2) - 2

readInput :: IO Orbits
readInput = parseData <$> readFileUtf8 "data/6"

part1 :: IO Int
part1 = calcOrbits <$> readInput

part2 :: IO Int
part2 = transfers <$> readInput
