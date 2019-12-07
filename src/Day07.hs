{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day07 where

import Data.Maybe
import RIO
import RIO.List (genericDrop, genericTake, genericIndex, permutations, uncons)
import RIO.List.Partial (head, last, maximum)
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

readArray :: [Integer] -> Integer -> Integer
readArray = genericIndex

writeArray :: [Integer] -> Integer -> Integer -> [Integer]
writeArray arr idx val =
  genericTake idx arr <> (val : genericDrop (idx + 1) arr)

run :: [Integer] -> [Integer] -> Integer -> [Integer]
run input arr pc = let
  (ins, imm1, imm2, _) = decodeIns $ readArray arr pc
  write addr v = writeArray arr addr v
  readAddr addr = readArray arr addr
  readLeal addr = readArray arr (readArray arr addr)
  read imm = if imm then readAddr else readLeal
  binop op = let
          src1 = read imm1 (pc + 1)
          src2 = read imm2 (pc + 2)
          dest = read True (pc + 3)
          in write dest (src1 `op` src2)
  cmp op = let
          src1 = read imm1 (pc + 1)
          src2 = read imm2 (pc + 2)
          dest = read True (pc + 3)
          in if src1 `op` src2
            then write dest 1
            else write dest 0

  in case ins of
    1 -> let
      -- add
      next = binop (+)
      in run input next (pc + 4)
    2 -> let
      -- mul
      next = binop (*)
      in run input next (pc + 4)
    3 -> let
      -- read
      (x, xs) = fromJust $ uncons input
      dest = readAddr (pc + 1)
      next = write dest x
      in run xs next (pc + 2)
    4 -> let
      -- write
      val = read imm1 (pc + 1)
      in val : run input arr (pc + 2)
    5 -> let
      -- jt
      val = read imm1 (pc + 1)
      dest = read imm2 (pc + 2)
      in if val > 0
        then run input arr dest
        else run input arr (pc + 3)
    6 -> let
      -- jf
      val = read imm1 (pc + 1)
      dest = read imm2 (pc + 2)
      in if val == 0
        then run input arr dest
        else run input arr (pc + 3)
    7 -> let
      next = cmp (<)
      in run input next (pc + 4)
    8 -> let
      next = cmp (==)
      in run input next (pc + 4)
    99 -> []
      -- ret
    _ -> []

exec :: [Integer] -> [Integer] -> [Integer]
exec input program = run input program 0

runInSeries :: [Integer] -> Integer -> [Integer] -> Integer
runInSeries _ input [] = input
runInSeries program input (x : xs) =
  let res = head $ exec (x : input : []) program
   in runInSeries program res xs

runInFeedback :: [Integer] -> [Integer] -> Integer
runInFeedback program [s1, s2, s3, s4, s5] =
  let p1i = s1 : 0 : p5res
      p2i = s2 : p1res
      p3i = s3 : p2res
      p4i = s4 : p3res
      p5i = s5 : p4res
      p1res = exec p1i program
      p2res = exec p2i program
      p3res = exec p3i program
      p4res = exec p4i program
      p5res = exec p5i program
   in last p5res
runInFeedback _ _ = undefined

findBest :: [Integer] -> [Integer] -> Integer
findBest program settings =
  maximum $
    runInSeries program 0
      <$> (permutations settings)

findBestFeedback :: [Integer] -> [Integer] -> Integer
findBestFeedback program settings =
  maximum $
    runInFeedback program
      <$> (permutations settings)


part1 :: IO Integer
part1 = do
  program <- getInput
  return $ findBest program [0 .. 4]

part2 :: IO Integer
part2 = do
  program <- getInput
  return $ findBestFeedback program [5 .. 9]
