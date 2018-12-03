module Day01 (day01) where

import Data.Foldable (fold)
import Data.Monoid (Sum(..))
import qualified Data.Set as Set

import Types

parseVal :: String -> Sum Integer
parseVal ('+':s) = parseVal s
parseVal s = Sum $ read s

findRepeat :: (Monoid a, Ord a) => [a] -> a
findRepeat = findRepeat' Set.empty . scanl (<>) mempty . cycle
  where findRepeat' _ [] = mempty
        findRepeat' s (x:xs) = if Set.member x s
                               then x
                               else findRepeat' (Set.insert x s) xs

day01 :: String -> Showable
day01 s = let vals = map parseVal $ lines s
              x = getSum $ fold vals
              y = getSum $ findRepeat vals
          in pack (x, y)
