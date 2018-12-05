module Main (main) where

import Data.Map.Lazy (lookupMax)
import System.Environment (getArgs)

import Solutions

evalPrint :: Maybe Int -> IO ()
evalPrint n = getSolution n >>= putSolution n

main :: IO ()
main = getArgs >>= go
  where go [] = evalPrint (fst <$> lookupMax solutionMap)
        go xs = mapM_ (evalPrint . Just . read) xs
