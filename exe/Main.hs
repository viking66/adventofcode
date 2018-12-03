module Main (main) where

import Prelude hiding (lookup)

import Data.Map.Lazy (fromList, lookup, lookupMax, Map)
import System.Environment (getArgs)

import Types
import Day01 (day01)
import Day02 (day02)
import Day03 (day03)

solutionMap :: Map Int (String -> Showable, String)
solutionMap = fromList
  [ (1, (day01, "data/day01.txt"))
  , (2, (day02, "data/day02.txt"))
  , (3, (day03, "data/day03.txt"))
  ]

getSolution :: Maybe Int -> IO Showable
getSolution = maybe noSol getSolution'
  where noSol :: IO Showable
        noSol = pure $ pack "No solution available"
        eval :: (String -> Showable) -> String -> IO Showable
        eval f s = readFile s >>= pure . f
        getSolution' :: Int -> IO Showable
        getSolution' n = maybe noSol (uncurry eval) (lookup n solutionMap)

main :: IO ()
main = getArgs >>= go
-- main = getArgs >>= go >>= mapM_ putShowable
  where go [] = getSolution (fst <$> lookupMax solutionMap) >>= putShowable
        go xs = mapM_ ((>>= putShowable) . getSolution . Just . read) xs
