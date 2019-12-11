{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day11
  ( exec,
    part1,
    part2,
  )
where

import Data.Maybe
import RIO
import qualified RIO.Map as M
import RIO.Partial (toEnum)
import RIO.State
import qualified RIO.Text as T
import System.IO
import Util (minmax, readInt)

getInput :: IO [Integer]
getInput = do
  input <- T.split (== ',') <$> readFileUtf8 "data/11"
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

data Direction = DUp | DDown | DLeft | DRight deriving (Show)

data Color = Black | White deriving (Show)

type Point = (Integer, Integer)

type Hull = M.Map Point Color

dstep :: Direction -> Point -> Point
dstep DUp = first (subtract 1)
dstep DDown = first (+ 1)
dstep DLeft = second (subtract 1)
dstep DRight = second (+ 1)

turn :: Integer -> Direction -> Direction
turn 1 d = case d of
  DUp -> DRight
  DRight -> DDown
  DDown -> DLeft
  DLeft -> DUp
turn _ d = case d of
  DUp -> DLeft
  DRight -> DUp
  DDown -> DRight
  DLeft -> DDown

data S
  = S
      { base :: Integer,
        pc :: Integer,
        mem :: Memory,
        direction :: Direction,
        hull :: Hull,
        pos :: Point,
        midInput :: Bool
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

run :: State S [Integer]
run = do
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
    1 -> binop (+) >> run
    -- mul
    2 -> binop (*) >> run
    -- input
    3 -> do
      x <- popInput
      writeIns imm1 1 x
      step 2
      run
    -- output
    4 -> do
      val <- readIns imm1 1
      step 2
      pushOutput val
      run
    -- jt
    5 -> do
      val <- readIns imm1 1
      dest <- readIns imm2 2
      if val /= 0
        then jmp dest
        else step 3
      run
    -- jf
    6 -> do
      val <- readIns imm1 1
      dest <- readIns imm2 2
      if val == 0
        then jmp dest
        else step 3
      run
    7 -> cmp (<) >> run
    8 -> cmp (==) >> run
    9 -> do
      val <- readIns imm1 1
      addRel val
      step 2
      run
    99 -> pure []
    _ -> error ("unknown instruction " <> show ins)

popInput :: State S Integer
popInput = do
  p <- pos <$> get
  h <- hull <$> get
  pure $ case M.lookup p h of
    Just White -> 1
    _ -> 0

pushOutput :: Integer -> State S ()
pushOutput o = do
  mid <- midInput <$> get
  if mid
    then do
      d <- direction <$> get
      p <- pos <$> get
      let nextDir = turn o d
      let nextPos = dstep nextDir p
      modify $ \s -> s {direction = nextDir, pos = nextPos, midInput = False}
    else do
      let c = if o == 1 then White else Black
      p <- pos <$> get
      h <- hull <$> get
      modify (\s -> s {hull = M.insert p c h, midInput = True})

initState :: Color -> [Integer] -> S
initState color program =
  let p = (10, 10)
   in S
        { midInput = False,
          mem = M.fromList (zip [0 ..] program),
          pc = 0,
          base = 0,
          hull = M.singleton p color,
          pos = p,
          direction = DUp
        }

exec :: Color -> [Integer] -> Hull
exec color program = hull $ execState run (initState color program)

part1 :: IO Int
part1 = do
  program <- getInput
  let res = exec Black program
  pure . length $ res

toInt :: Integer -> Int
toInt = fromIntegral

part2 :: IO Text
part2 = do
  program <- getInput
  let res = exec White program
  let indices = M.keys res
  let (minx, maxx) = minmax (toInt . fst <$> indices)
  let (miny, maxy) = minmax (toInt . snd <$> indices)
  forM_ [minx .. maxx] $ \x -> do
    forM_ [miny .. maxy] $ \y -> do
      putStr $ case M.lookup (fromIntegral x, fromIntegral y) res of
        Just White -> "█"
        _ -> " "
    putStrLn ""
  pure $ "UERPRFGJ"
