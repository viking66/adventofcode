module Day08 (day08) where

import Data.Tree

import Types

type IntTree = Tree [Int]

makeTree :: [Int] -> (IntTree, [Int])
makeTree (0:y:ys) = (Node (take y ys) [], drop y ys)
makeTree (x:y:ys) = let (ts, zs) = makeForest x ys
                    in (Node (take y zs) ts, drop y zs)
  where makeForest 0 xs = ([], xs)
        makeForest n xs = let (t, ys) = makeTree xs
                          in appendTree t (makeForest (n - 1) ys)
        appendTree t (ts, ns) = (t:ts, ns)
makeTree _ = (Node [] [], [])

nodeValue :: IntTree -> Int
nodeValue (Node xs []) = sum xs
nodeValue (Node xs ys) = sum (map (childValue ys) xs)
  where childValue xs n = if n > length xs
                          then 0
                          else nodeValue (xs !! (n - 1))

parseInput :: String -> [Int]
parseInput = map read . words

go :: [Int] -> (Int, Int)
go xs = let t = fst (makeTree xs)
        in (foldTree (\as bs -> sum (as++bs)) t, nodeValue t)

day08 :: String -> Showable
day08 = pack . go . parseInput
