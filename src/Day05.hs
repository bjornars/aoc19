{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day05 where

import Data.Array.ST
import Data.Maybe
import RIO
import RIO.List (uncons)
import qualified RIO.Text as T
import Util (readInt)

getInput :: IO [Integer]
getInput = do
  input <- T.split (== ',') <$> readFileUtf8 "data/5"
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

part1 :: IO Integer
part1 = do
  program <- getInput
  let res = reverse $ exec [1] program
  case uncons res of
    Just (code, successes) -> do
      if any (/=0) successes
        then fail "omg"
        else return code
    Nothing -> fail "omg"

part2 :: IO Integer
part2 = do
  program <- getInput
  let res = exec [5] program
  case res of
    [code] -> return code
    _ -> fail "omg"
