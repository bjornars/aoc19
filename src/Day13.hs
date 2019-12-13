{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day13
  ( execWith,
    part1,
    part2,
  )
where

import Data.Maybe
import Data.Tuple (swap)
import RIO
import qualified RIO.Map as M
import RIO.Partial (toEnum)
import RIO.State
import qualified RIO.Text as T
import System.IO (getChar, putStr, putStrLn)
import Util (minmax, readInt)

getInput :: IO [Integer]
getInput = do
  input <- T.split (== ',') <$> readFileUtf8 "data/13"
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

data Cell = Empty | Wall | Block | Paddle | Ball deriving (Enum, Eq)

instance Show Cell where
  show Empty = "  "
  show Wall = "##"
  show Block = "xx"
  show Paddle = "--"
  show Ball = "<>"

type Point = (Integer, Integer)

type Board = M.Map Point Cell

data Buffer = Done | Got1 Integer | Got2 Integer Integer deriving (Show)

type SIO a = StateT S IO a

data S
  = S
      { base :: Integer,
        pc :: Integer,
        mem :: Memory,
        board :: Board,
        buffer :: Buffer,
        isAI :: Bool,
        score :: Integer
      }
  deriving (Show)

readS :: Integer -> SIO Integer
readS idx | idx < 0 = error $ "SIGSEGV: " <> show idx
readS idx = do
  mem' <- mem <$> get
  pure . fromMaybe 0 $ M.lookup idx mem'

readIns :: Mode -> Integer -> SIO Integer
readIns mode idx = do
  pc' <- pc <$> get
  read mode $ pc' + idx

read :: Mode -> Integer -> SIO Integer
read mode idx = do
  offset <- base <$> get
  case mode of
    Position -> readS idx >>= readS
    Immediate -> readS idx
    Relative -> do
      target <- readS idx
      readS (target + offset)

writeIns :: Mode -> Integer -> Integer -> SIO ()
writeIns mode idx val = do
  pc' <- pc <$> get
  offset <- base <$> get
  case mode of
    Position -> readS (pc' + idx) >>= flip write val
    Immediate -> error "immediate write"
    Relative -> do
      target <- readS (pc' + idx)
      write (target + offset) val

write :: Integer -> Integer -> SIO ()
write idx _ | idx < 0 = error $ "SIGSEGV" <> show idx
write idx val = do
  modify (\s -> s {mem = M.insert idx val (mem s)})

step :: Integer -> SIO ()
step n = modify $ \s -> s {pc = n + pc s}

jmp :: Integer -> SIO ()
jmp n = modify $ \s -> s {pc = n}

addRel :: Integer -> SIO ()
addRel n = modify $ \s -> s {base = n + base s}

run :: SIO [Integer]
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

popInput :: SIO Integer
popInput = do
  ai' <- isAI <$> get
  if ai'
    then ai
    else do
      showBoard
      liftIO getChar >>= \case
        'a' -> pure (-1)
        'd' -> pure 1
        ' ' -> pure 0
        _ -> ai

ai :: SIO Integer
ai = do
  b <- fmap swap . M.toList . board <$> get
  let paddle = fromJust $ lookup Paddle b
  let ball = fromJust $ lookup Ball b
  pure $ case fst paddle `compare` fst ball of
    GT -> (-1)
    EQ -> 0
    LT -> 1

pushOutput :: Integer -> SIO ()
pushOutput o = do
  buffer <$> get >>= \case
    Done -> modify (\s -> s {buffer = Got1 o})
    Got1 x -> modify (\s -> s {buffer = Got2 x o})
    Got2 x y -> do
      b <- board <$> get
      if (x, y) == (-1, 0)
        then do
          modify (\s -> s {buffer = Done, score = o})
        else do
          let b' = M.insert (x, y) (toEnum $ fromIntegral o) b
          modify (\s -> s {board = b', buffer = Done})

initState :: [Integer] -> S
initState program = S
  { mem = M.fromList (zip [0 ..] program),
    pc = 0,
    base = 0,
    board = M.empty,
    buffer = Done,
    isAI = True,
    score = 0
  }

execWith :: (Integer -> Integer) -> [Integer] -> IO S
execWith f (p : ps) = do
  let program = f p : ps
  res <- execStateT run (initState program)
  pure $ res
execWith _ [] = error "bleg"

part1 :: IO Int
part1 = do
  program <- getInput
  res <- board <$> execWith id program
  let numBlocks = length . filter (== Block) . M.elems $ res
  pure numBlocks

toInt :: Integer -> Int
toInt = fromIntegral

showBoard :: SIO ()
showBoard = do
  res <- board <$> get
  let indices = M.keys res
  let (minx, maxx) = minmax (toInt . fst <$> indices)
  let (miny, maxy) = minmax (toInt . snd <$> indices)
  forM_ [miny .. maxy] $ \y -> do
    forM_ [minx .. maxx] $ \x -> do
      liftIO . putStr . show . fromMaybe Empty $ M.lookup (fromIntegral x, fromIntegral y) res
    liftIO . putStrLn $ ""

part2 :: IO Integer
part2 = do
  program <- getInput
  res <- execWith (const 2) program
  pure $ score res
