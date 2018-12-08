module Day08 (day08) where

import Data.Tree

import Types

type IntTree = Tree [Int]

foo :: [Int] -> (IntTree, [Int])
foo (0:y:ys) = (Node (take y ys) [], drop y ys)
foo (x:y:ys) = let (ts, zs) = bar x ys
               in (Node (take y zs) ts, drop y zs)

bar :: Int -> [Int] -> ([IntTree], [Int])
bar 0 xs = ([], xs)
bar n xs = let (t, ys) = foo xs
           in baz t (bar (n - 1) ys)

baz :: IntTree -> ([IntTree], [Int]) -> ([IntTree], [Int])
baz t (ts, ns) = (t:ts, ns)

qux :: [Int] -> [Int] -> Int
qux as bs = sum (as ++ bs)

rambutan :: [IntTree] -> Int -> Int
rambutan xs n = if n > length xs
                then 0
                else bam (xs !! (n - 1))

bam :: IntTree -> Int
bam (Node xs []) = sum xs
bam (Node xs ys) = sum (map (rambutan ys) xs)

parseInput :: String -> [Int]
parseInput = map read . words

go :: [Int] -> (Int, Int)
go xs = let t = fst (foo xs)
        in (foldTree qux t, bam t)

day08 :: String -> Showable
day08 = pack . go . parseInput
