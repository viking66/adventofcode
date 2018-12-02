module Main (main) where

import Data.Map.Lazy (findWithDefault, fromList, lookupMax, Map)
import System.Environment (getArgs)

import Day01 (day01)
import Day02 (day02)

data Solution a = Solution a | SolutionIO (IO a)

solutions :: Map Int (Solution String)
solutions = fromList
  [ (1, solutionIO1 day01 (readFile "data/day1.txt"))
  , (2, solutionIO1 day02 (readFile "data/day2.txt"))
  ]

solutionIO1 :: Show b => (a -> b) -> IO a -> Solution String
solutionIO1 f a = SolutionIO (show . f <$> a)

getSolution :: Int -> (Int, Solution String)
getSolution n = (n, findWithDefault unsolved n solutions)
  where unsolved = Solution "No solution implemented."

putSolution :: Int -> Solution String -> IO ()
putSolution n sol = case sol of
                      (Solution s) -> putSolution' n s
                      (SolutionIO s) -> s >>= (putSolution' n)
  where putSolution' n s = putStrLn $ show n ++ ": " ++ s

main :: IO ()
main = getArgs >>= go
  where go [] = maybe (putStrLn "No solutions.") (uncurry putSolution) $ lookupMax solutions
        go xs = mapM_ (uncurry putSolution . getSolution . read) xs
