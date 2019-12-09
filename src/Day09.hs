{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day09
  ( exec,
    part1,
    part2,
  )
where

import Data.Maybe
import RIO
import RIO.List (uncons)
import RIO.List.Partial (head)
import qualified RIO.Map as M
import RIO.Partial (toEnum)
import RIO.State
import qualified RIO.Text as T
import Util (readInt)

getInput :: IO [Integer]
getInput = do
  input <- T.split (== ',') <$> readFileUtf8 "data/9"
  let parsed = fromJust $ traverse readInt input
  return $ parsed

type Memory = M.Map Integer Integer

data Mode = Position | Immediate | Relative deriving (Show, Eq, Enum)

decodeIns :: Integer -> (Integer, Mode, Mode, Mode)
decodeIns 99 = (99, Position, Position, Position)
decodeIns d =
  let (n1, ins) = fromIntegral d `divMod` 100
      (n2, imm1) = n1 `divMod` 10
      (n3, imm2) = n2 `divMod` 10
      (_, imm3) = n3 `divMod` 10
   in (fromIntegral ins, toEnum imm1, toEnum imm2, toEnum imm3)

data S
  = S
      { base :: Integer,
        pc :: Integer,
        mem :: Memory
      }
  deriving (Show)

readS :: Integer -> State S Integer
readS idx | idx < 0 = error $ "SIGSEGV: " <> show idx
readS idx = do
  mem' <- mem <$> get
  pure . fromMaybe 0 $ M.lookup idx mem'

readIns :: Mode -> Integer -> State S Integer
readIns mode idx = do
  pc' <- pc <$> get
  read mode $ pc' + idx

read :: Mode -> Integer -> State S Integer
read mode idx = do
  offset <- base <$> get
  case mode of
    Position -> readS idx >>= readS
    Immediate -> readS idx
    Relative -> do
      target <- readS idx
      readS (target + offset)

writeIns :: Mode -> Integer -> Integer -> State S ()
writeIns mode idx val = do
  pc' <- pc <$> get
  offset <- base <$> get
  case mode of
    Position -> readS (pc' + idx) >>= flip write val
    Immediate -> error "immediate write"
    Relative -> do
      target <- readS (pc' + idx)
      write (target + offset) val

write :: Integer -> Integer -> State S ()
write idx _ | idx < 0 = error $ "SIGSEGV" <> show idx
write idx val = do
  modify (\s -> s {mem = M.insert idx val (mem s)})

step :: Integer -> State S ()
step n = modify $ \s -> s {pc = n + pc s}

jmp :: Integer -> State S ()
jmp n = modify $ \s -> s {pc = n}

addRel :: Integer -> State S ()
addRel n = modify $ \s -> s {base = n + base s}

run :: [Integer] -> State S [Integer]
run input = do
  (ins, imm1, imm2, imm3) <- decodeIns <$> readIns Immediate 0
  let binop op =
        do
          src1 <- readIns imm1 1
          src2 <- readIns imm2 2
          writeIns imm3 3 (src1 `op` src2)
          step 4
  let cmp op =
        do
          src1 <- readIns imm1 1
          src2 <- readIns imm2 2
          writeIns imm3 3 $
            if src1 `op` src2
              then 1
              else 0
          step 4
  case ins of
    -- add
    1 -> binop (+) >> run input
    -- mul
    2 -> binop (*) >> run input
    -- input
    3 -> do
      let (x, xs) = fromJust $ uncons input
      writeIns imm1 1 x
      step 2
      run xs
    -- output
    4 -> do
      val <- readIns imm1 1
      step 2
      (val :) <$> run input
    -- jt
    5 -> do
      val <- readIns imm1 1
      dest <- readIns imm2 2
      if val /= 0
        then jmp dest
        else step 3
      run input
    -- jf
    6 -> do
      val <- readIns imm1 1
      dest <- readIns imm2 2
      if val == 0
        then jmp dest
        else step 3
      run input
    7 -> cmp (<) >> run input
    8 -> cmp (==) >> run input
    9 -> do
      val <- readIns imm1 1
      addRel val
      step 2
      run input
    99 -> pure []
    _ -> error ("unknown instruction " <> show ins)

exec :: [Integer] -> [Integer] -> [Integer]
exec input program =
  let memory = M.fromList (zip [0 ..] program)
   in evalState (run input) (S {mem = memory, pc = 0, base = 0})

part1 :: IO Integer
part1 = do
  program <- getInput
  let res = exec [1] program
  return $ head res

part2 :: IO Integer
part2 = do
  program <- getInput
  let res = exec [2] program
  return $ head res
