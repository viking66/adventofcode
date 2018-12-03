module Main (main) where

import Prelude hiding (lookup)

import Data.Map.Lazy (fromList, lookup, lookupMax, Map)
import System.Environment (getArgs)
import Text.Printf (printf)

import Types
import Day01 (day01)
import Day02 (day02)
import Day03 (day03)

solutionMap :: Map Int (String -> Showable)
solutionMap = fromList
  [ (1, day01)
  , (2, day02)
  , (3, day03)
  -- , (4, day04)
  -- , (5, day05)
  -- , (6, day06)
  -- , (7, day07)
  -- , (8, day08)
  -- , (9, day09)
  -- , (10, day10)
  -- , (11, day11)
  -- , (12, day12)
  -- , (13, day13)
  -- , (14, day14)
  -- , (15, day15)
  -- , (16, day16)
  -- , (17, day17)
  -- , (18, day18)
  -- , (19, day19)
  -- , (20, day20)
  -- , (21, day21)
  -- , (22, day22)
  -- , (23, day23)
  -- , (24, day24)
  -- , (25, day25)
  ]

getFile :: Int -> String
getFile = printf "data/day%02d.txt"

getSolution :: Maybe Int -> IO Showable
getSolution = maybe noSol getSolution'
  where noSol :: IO Showable
        noSol = pure $ pack "No solution available"
        eval :: Int -> (String -> Showable) -> IO Showable
        eval n f = readFile (getFile n) >>= pure . f
        getSolution' :: Int -> IO Showable
        getSolution' n = maybe noSol (eval n) (lookup n solutionMap)

putSolution :: Maybe Int -> Showable -> IO ()
putSolution Nothing s = putStrLn $ show s
putSolution (Just n) s = putStrLn $ show n ++ ": " ++ show s

evalPrint :: Maybe Int -> IO ()
evalPrint n = getSolution n >>= putSolution n

main :: IO ()
main = getArgs >>= go
  where go [] = evalPrint (fst <$> lookupMax solutionMap)
        go xs = mapM_ (evalPrint . Just . read) xs
