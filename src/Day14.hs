module Day14 (day14) where

import Data.Char (digitToInt)
import Data.Foldable (toList)
import Data.List (isInfixOf, isPrefixOf)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq

import Parser hiding (digits)
import Types

data Recipes = Recipes (Seq Int) Int Int
  deriving Show

size :: Recipes -> Int
size (Recipes s _ _) = Seq.length s

recipesToList :: Recipes -> [Int]
recipesToList (Recipes s _ _) = toList s

digits :: Int -> [Int]
digits = map digitToInt . show

initRecipes :: Recipes
initRecipes = Recipes (Seq.fromList [3,7]) 0 1

updateRecipes :: Recipes -> Recipes
updateRecipes (Recipes s e1 e2) =
  let move t e = ((Seq.index t e) + 1 + e) `mod` (Seq.length t)
      xs = digits $ (Seq.index s e1) + (Seq.index s e2)
      s' = foldl (|>) s xs
  in Recipes s' (move s' e1) (move s' e2)

go1 :: Int -> String
go1 = concatMap show . toList . go1' initRecipes
  where go1' r@(Recipes s _ _) n
          | Seq.length s >= (n + 10) = Seq.take 10 $ Seq.drop n s
          | otherwise = go1' (updateRecipes r) n

go2 :: Int -> Int
go2 n = go2' (digits n) increment
  where increment :: Int
        increment = 1000000
        rs = iterate updateRecipes initRecipes
        go2' ns a = let r = head $ dropWhile ((< a) . size) rs
                    in if hasTarget ns r
                       then count 0 ns (recipesToList r)
                       else go2' ns (a + increment)
        hasTarget t r = isInfixOf t (recipesToList r)
        count _ _ [] = 0
        count n ns rss@(_:rs) = if isPrefixOf ns rss
                                then n
                                else count (succ n) ns rs

go :: Int -> (String, Int)
go n = (go1 n, go2 n)

parseInput :: String -> Maybe Int
parseInput = (fst <$>) . runParser number . filter (/= '\n')

day14 :: String -> Showable
day14 = maybe (pack "Failed to parse input") (pack . go) . parseInput
