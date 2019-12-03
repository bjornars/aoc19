{-# LANGUAGE NoImplicitPrelude #-}

module Util where

import RIO
import qualified RIO.Text as T

asFloat :: Integer -> Double
asFloat = fromIntegral

readInt :: Text -> Maybe Integer
readInt = T.unpack >>> readMaybe

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

chunk :: Int -> [a] -> [[a]]
chunk n xs =
  let (a, b) = splitAt n xs
   in if null b
        then [a]
        else a : chunk n b

distance :: Num a => (a, a) -> (a, a) -> a
distance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)
