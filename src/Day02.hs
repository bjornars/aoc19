{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day02 where

import Data.Array.ST
import Data.Maybe
import RIO
import RIO.List.Partial ((!!))
import qualified RIO.Text as T
import Util (readInt)

data Op
  = Add Integer Integer Integer
  | Mul Integer Integer Integer
  | HCF
  deriving (Eq, Show)

getInput :: IO [Integer]
getInput = do
  input <- T.split (== ',') <$> readFileUtf8 "data/2"
  let parsed = fromJust $ traverse readInt input
  return $ parsed

run :: forall s. STArray s Integer Integer -> Integer -> ST s ()
run arr pc = do
  ins <- readArray arr pc
  let read addr = readArray arr addr
  let rread addr = readArray arr addr >>= readArray arr
  let write addr v = writeArray arr addr v
  case ins of
    1 -> do
      src1 <- rread (pc + 1)
      src2 <- rread (pc + 2)
      dest <- read (pc + 3)
      write dest (src1 + src2)
      run arr (pc + 4)
    2 -> do
      src1 <- rread (pc + 1)
      src2 <- rread (pc + 2)
      dest <- read (pc + 3)
      write dest (src1 * src2)
      run arr (pc + 4)
    99 -> do
      return ()
    _ -> do
      return ()

exec :: [Integer] -> (forall s. STArray s Integer Integer -> ST s ()) -> [Integer]
exec program setup =
  toList
    $ runSTArray
    $ do
      arr <- newListArray (0, fromIntegral $ length program - 1) program
      setup arr
      run arr 0
      return arr

execWith :: [Integer] -> Integer -> Integer -> [Integer]
execWith input a b = exec input $ \arr -> do
  writeArray arr 1 a
  writeArray arr 2 b

part1 :: IO Integer
part1 = do
  input <- getInput
  let res = execWith input 12 02
  return $ res !! 0

part2 :: IO Integer
part2 = do
  input <- getInput
  let (x, y) = fromJust $ lookup 19690720 (res input)
  return $ x * 100 + y
  where
    res input = do
      x <- [0 .. 100]
      y <- [0 .. 100]
      return $ (execWith input x y !! 0, (x, y))
