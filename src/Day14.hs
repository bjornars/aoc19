{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day14 where

import Data.Char (isUpper)
import Data.Maybe (fromJust)
import qualified Parser as P
import RIO
import qualified RIO.Map as M
import qualified RIO.State as S
import qualified RIO.Text as T

data Reagent = Reagent Int Text deriving (Show, Eq)

data Reaction
  = Reaction
      [Reagent] -- inputs
      Reagent -- outputs
  deriving (Show, Eq)

rmul :: Int -> Reagent -> Reagent
rmul m (Reagent n t) = Reagent (n * m) t

parseText :: P.ReadP Text
parseText = T.pack <$> some (P.satisfy isUpper)

parseReagent :: P.ReadP Reagent
parseReagent =
  let n = P.nonNegDigit
      text = P.char ' ' *> parseText
   in Reagent <$> n <*> text

parseReaction :: P.ReadP Reaction
parseReaction = Reaction <$> (P.sepBy parseReagent (P.string ", ")) <*> (P.string " => " *> parseReagent)

parser :: P.ReadP [Reaction]
parser = P.endBy parseReaction (P.char '\n')

getInput :: IO (Maybe [Reaction])
getInput = do
  i <- readFileUtf8 "data/14"
  pure $ P.parse (parser <* P.eof) i

type Recipe = M.Map Text Reaction

buildRecipe :: [Reaction] -> Recipe
buildRecipe = fmap (\r@(Reaction _ (Reagent _ name)) -> (name, r)) >>> M.fromList

getCycles :: Int -> Int -> (Int, Int)
getCycles total per =
  let (n, x) = total `divMod` per
   in if x == 0
        then (n, 0)
        else (n + 1, per - x)

getSpare :: Text -> Int -> M.Map Text Int -> (M.Map Text Int, Int)
getSpare name n m = case M.lookup name m of
  Just v | v <= n -> (M.delete name m, n - v)
  Just _ | otherwise -> (M.adjust (subtract n) name m, 0)
  Nothing -> (m, n)

data S
  = S
      { recipe :: Recipe,
        spare :: !(M.Map Text Int),
        ore :: !Int
      }
  deriving (Show)

type StateS a = S.State S a

build :: [Reagent] -> StateS ()
build [] = pure ()
build (Reagent n "ORE" : xs) = S.modify (\s -> s {ore = ore s + n}) >> build xs
build (Reagent n name : xs) = do
  recipe' <- S.gets recipe
  spare' <- S.gets spare
  let (Reaction inputs (Reagent per _)) = fromJust $ M.lookup name recipe'
  let (spare'', neededAfterSpare) = getSpare name n spare'
  let (cycles, leftover) = getCycles neededAfterSpare per
  let spare''' =
        if leftover > 0
          then M.alter (fromMaybe 0 >>> (+ leftover) >>> Just) name spare''
          else spare''
  let req = if cycles == 0 then [] else rmul cycles <$> inputs
  let (req', spare'''') = fillFromSpare req spare'''
  S.modify $ \s -> s {spare = spare''''}
  build (req' <> xs)

fillFromSpare :: [Reagent] -> M.Map Text Int -> ([Reagent], M.Map Text Int)
fillFromSpare [] m = ([], m)
fillFromSpare (x@(Reagent nreq name) : xs) m = case M.lookup name m of
  (Just nspare) -> case compare nreq nspare of
    LT -> (xs, M.adjust (subtract nreq) name m)
    EQ -> (xs, M.delete name m)
    GT -> first (Reagent (nreq - nspare) name :) $ fillFromSpare xs (M.delete name m)
  Nothing -> first (x :) $ fillFromSpare xs m

initRecipe :: [Reaction] -> S
initRecipe inputs = S {recipe = buildRecipe inputs, spare = M.empty, ore = 0}

exec :: Int -> [Reaction] -> Int
exec n inputs = ore $ flip S.execState (initRecipe inputs) (build [Reagent n "FUEL"])

part1 :: IO Int
part1 = do
  inputs <- fromJust <$> getInput
  pure $ exec 1 inputs

binarySearch :: (Int -> Bool) -> Int -> Int -> Int
binarySearch _ lo hi | lo + 1 == hi = lo
binarySearch pred lo hi =
  let avg = (lo + hi) `div` 2
   in if pred avg
        then binarySearch pred avg hi
        else binarySearch pred lo avg

findN :: [Reaction] -> Int -> Int
findN inputs n =
  let ans = exec 1 inputs
      guess = n `div` ans
   in binarySearch (\g -> exec g inputs < n) (guess `div` 2) (guess * 2)

part2 :: IO Int
part2 = do
  inputs <- fromJust <$> getInput
  pure $ findN inputs 1000000000000
