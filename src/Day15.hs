{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day15
  ( execWith,
    showBoard,
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
  input <- T.split (== ',') <$> readFileUtf8 "data/15"
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

data Cell = Empty | Wall | Oxygen | Unknown deriving (Eq)

instance Show Cell where
  show Empty = " "
  show Wall = "X"
  show Oxygen = "*"
  show Unknown = "-"

type Point = (Integer, Integer)

type Board = M.Map Point Cell

data Buffer = Done | Got1 Integer | Got2 Integer Integer deriving (Show)

data Direction = DirXX | DirN | DirS | DirW | DirE deriving (Enum, Show)

data PMode = Blocked | Moving deriving (Show)

type SIO a = StateT S IO a

data S
  = S
      { base :: Integer,
        pc :: Integer,
        mem :: Memory,
        board :: Board,
        current :: Point,
        pmode :: PMode,
        dir :: Direction
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
      position <- gets current
      if (position /= (0, 0)) then run else pure []
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

turnLeft :: Direction -> Direction
turnLeft DirN = DirE
turnLeft DirE = DirS
turnLeft DirS = DirW
turnLeft DirW = DirN
turnLeft DirXX = DirXX

turnRight :: Direction -> Direction
turnRight = turnLeft . turnLeft . turnLeft

popInput :: SIO Integer
popInput = do
  mode' <- gets pmode
  dir' <- gets dir
  let nextDir =
        case mode' of
          Moving -> turnLeft dir'
          Blocked -> turnRight dir'
  modify (\s -> s {dir = nextDir})
  -- traceShowM ("sending ", nextDir)
  pure . fromIntegral . fromEnum $ nextDir

move :: Point -> Direction -> Point
move (x, y) DirN = (x, y -1)
move (x, y) DirW = (x -1, y)
move (x, y) DirE = (x + 1, y)
move (x, y) __ = (x, y + 1)

pushOutput :: Integer -> SIO ()
pushOutput o = do
  -- traceShowM ("getting", o)
  pos' <- gets current
  dir' <- gets dir
  board' <- gets board
  let target = move pos' dir'
  case o of
    -- wall
    0 -> do
      modify (\s -> s {board = M.insert target Wall board', pmode = Blocked})
    -- open
    1 -> do
      modify (\s -> s {board = M.insert target Empty board', current = target, pmode = Moving})
    -- oxygen
    2 -> do
      modify (\s -> s {board = M.insert target Oxygen board', current = target, pmode = Moving})
    _ -> pure ()

--showBoard
-- void $ liftIO getChar
-- liftIO $ putStrLn ""

initState :: [Integer] -> S
initState program = S
  { current = (0, 0),
    pmode = Moving,
    dir = DirN,
    mem = M.fromList (zip [0 ..] program),
    pc = 0,
    base = 0,
    board = M.empty
  }

execWith :: (Integer -> Integer) -> [Integer] -> IO S
execWith f (p : ps) = do
  let program = f p : ps
  res <- execStateT run (initState program)
  pure $ res
execWith _ [] = error "bleg"

part1 :: IO Integer
part1 = do
  program <- getInput
  res <- execWith id program
  -- execStateT showBoard res
  pure $ bfs (board res) [(0, 0)]

toInt :: Integer -> Int
toInt = fromIntegral

showBoard :: SIO ()
showBoard = do
  res <- gets board
  current' <- gets current
  let indices = M.keys res
  let (minx, maxx) = minmax (toInt . fst <$> indices)
  let (miny, maxy) = minmax (toInt . snd <$> indices)
  forM_ [miny .. maxy] $ \y -> do
    forM_ [minx .. maxx] $ \x -> do
      let p = (fromIntegral x, fromIntegral y)
      if p == current'
        then liftIO $ putStr "D"
        else liftIO . putStr . show . fromMaybe Unknown $ M.lookup p res
    liftIO . putStrLn $ ""
  liftIO . putStrLn $ "*******************"

bfs :: Board -> [Point] -> Integer
bfs b starts = go ((0,) <$> starts) []
  where
    go [] _ = -1
    go ((_, t) : ts) seen | t `elem` seen = go ts seen
    go ((l, t) : ts) seen = case M.lookup t b of
      Just Oxygen -> l
      Just Empty -> go (ts <> mk_alt l t) (t : seen)
      _ -> go ts (t : seen)
    mk_alt l t = (l + 1,) . move t <$> [DirE, DirN, DirW, DirS]

fill :: Board -> [Point] -> Integer
fill b starts = let res = go ((0,) <$> starts) [] 0 in res
  where
    go [] _ steps = steps
    go ((_, t) : ts) seen steps | t `elem` seen = go ts seen steps
    go ((l, t) : ts) seen steps = case M.lookup t b of
      Just Empty -> go (ts <> mk_alt l t) (t : seen) l
      Just Oxygen -> go (ts <> mk_alt l t) (t : seen) l
      _ -> go ts (t : seen) steps
    mk_alt l t = (l + 1,) . move t <$> [DirE, DirN, DirW, DirS]

part2 :: IO Integer
part2 = do
  program <- getInput
  res <- execWith id program
  let fn acc coords a = if a == Oxygen then coords else acc
  let oxy = M.foldlWithKey fn (0, 0) (board res)
  pure $ fill (board res) [oxy]
