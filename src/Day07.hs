{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day07 where

import Data.Array.ST
import Data.Maybe
import RIO
import RIO.List (permutations, uncons)
import RIO.List.Partial (last, maximum, head)
import qualified RIO.Text as T
import Util (readInt)

getInput :: IO [Integer]
getInput = do
  input <- T.split (== ',') <$> readFileUtf8 "data/7"
  let parsed = fromJust $ traverse readInt input
  return $ parsed

decodeIns :: Integer -> (Integer, Bool, Bool, Bool)
decodeIns 99 = (99, False, False, False)
decodeIns d =
  let (n1, ins) = d `divMod` 100
      (n2, imm1) = n1 `divMod` 10
      (n3, imm2) = n2 `divMod` 10
      (_, imm3) = n3 `divMod` 10
   in (ins, imm1 > 0, imm2 > 0, imm3 > 0)

run :: forall s. [Integer] -> STArray s Integer Integer -> Integer -> ST s [Integer]
run input arr pc = do
  (ins, imm1, imm2, _) <- decodeIns <$> readArray arr pc
  let write addr v = writeArray arr addr v
  let readAddr addr = readArray arr addr
  let readLeal addr = readArray arr addr >>= readArray arr
  let read imm = if imm then readAddr else readLeal

  let binop op =
        do
          src1 <- read imm1 (pc + 1)
          src2 <- read imm2 (pc + 2)
          dest <- read True (pc + 3)
          write dest (src1 `op` src2)

  let cmp op =
        do
          src1 <- read imm1 (pc + 1)
          src2 <- read imm2 (pc + 2)
          dest <- read True (pc + 3)
          if src1 `op` src2
            then write dest 1
            else write dest 0

  case ins of
    1 -> do -- add
      binop (+)
      run input arr (pc + 4)
    2 -> do -- mul
      binop (*)
      run input arr (pc + 4)
    3 -> do -- read
      let (x, xs) = fromJust $ uncons input
      dest <- readAddr (pc + 1)
      write dest x
      run xs arr (pc + 2)
    4 -> do -- write
      val <- read imm1 (pc + 1)
      (val:) <$> run input arr (pc + 2)
    5 -> do -- jt
      val <- read imm1 (pc + 1)
      dest <- read imm2 (pc + 2)
      if val > 0
        then run input arr dest
        else run input arr (pc + 3)
    6 -> do -- jf
      val <- read imm1 (pc + 1)
      dest <- read imm2 (pc + 2)
      if val == 0
        then run input arr dest
        else run input arr (pc + 3)
    7 -> do
      cmp (<)
      run input arr (pc + 4)
    8 -> do
      cmp (==)
      run input arr (pc + 4)
    99 -> do -- ret
      return []
    _ -> do
      return []

exec :: [Integer] -> [Integer] -> [Integer]
exec input program =
  toList
    $ runSTArray
    $ do
      arr <- newListArray (0, fromIntegral $ length program - 1) program
      res <- run input arr 0
      arr2 <- newListArray (0 :: Integer, fromIntegral $ length res -1) res
      return arr2

runInSeries :: [Integer] -> Integer -> [Integer] -> Integer
runInSeries _ input [] = input
runInSeries program input (x:xs) =
  let res = head $ exec (x:input:[]) program
  in runInSeries program res xs

runInFeedback program settings =
  let p1i = 0 : p5res
      p2i = p1res
      p3i = p2res
      p4i = p3res
      p5i = p4res
      p1res = exec p1i program
      p2res = exec p2i program
      p3res = exec p3i program
      p4res = exec p4i program
      p5res = exec p5i program
    in last $ p5res
 
findBest :: [Integer] -> [Integer] -> Integer
findBest program settings = maximum
  $ runInSeries program 0
  <$> (permutations settings)

part1 :: IO Integer
part1 = do
  program <- getInput
  return $ findBest program [0..4]

part2 :: IO Integer
part2 = do
  program <- getInput
  return $ findBest program [5..9]
