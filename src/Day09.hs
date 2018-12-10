{-# LANGUAGE FlexibleContexts #-}

module Day09 (day09) where

import qualified Data.Map.Lazy as Map

import Parser
import Types

data ZipList a = ZipList [a] a [a]

instance (Show a) => Show (ZipList a) where
  show = show . toList

type Players = Int
type Target = Int
data Game = Game Players Target
  deriving Show

makeZipList :: a -> ZipList a
makeZipList a = ZipList [] a []

toList :: ZipList a -> [a]
toList (ZipList xs y zs) = reverse xs ++ (y:zs)

focus :: ZipList a -> a
focus (ZipList _ a _) = a

moveLeft :: ZipList a -> ZipList a
moveLeft (ZipList (x:xs) y zs) = ZipList xs x (y:zs)
moveLeft (ZipList [] y zs) = let (x:xs) = reverse (y:zs)
                             in ZipList xs x []

moveLeftN :: Int -> ZipList a -> ZipList a
moveLeftN 0 z = z
moveLeftN n z = moveLeftN (n-1) (moveLeft z)

moveRight :: ZipList a -> ZipList a
moveRight (ZipList xs y (z:zs)) = ZipList (y:xs) z zs
moveRight (ZipList xs y []) = let (z:zs) = reverse (y:xs)
                              in ZipList [] z zs

insertFocus :: a -> ZipList a -> ZipList a
insertFocus a (ZipList xs y zs) = ZipList (y:xs) a zs

removeFocus :: ZipList a -> Maybe (ZipList a)
removeFocus (ZipList [] _ []) = Nothing
removeFocus (ZipList xs _ (z:zs)) = Just $ ZipList xs z zs
removeFocus (ZipList xs _ []) = let (z:zs) = reverse xs
                                in Just $ ZipList [] z zs

insertMarble :: a -> ZipList a -> ZipList a
insertMarble a = insertFocus a . moveRight

handleMarble23 :: ZipList a -> (a, Maybe (ZipList a))
handleMarble23 zs = let zs' = moveLeftN 7 zs
                    in (focus zs', removeFocus zs')

maxScore :: [(Int, Int)] -> Int
maxScore = maximum . Map.elems . Map.fromListWith (+)

updateTarget :: (Target -> Target) -> Game -> Game
updateTarget f (Game p t) = Game p (f t)

go :: Game -> Maybe Int
go (Game players target) = go' 1 players target [] (makeZipList 0)
  where go' n p t ss zs
          | n > t = Just $ maxScore ss
          | n `mod` 23 == 0 = let (x, zs') = handleMarble23 zs
                                  points = x + n
                                  ss' = ((n `mod` p), points) : ss
                              in maybe Nothing (go' (succ n) p t ss') zs'
          | otherwise       = go' (succ n) p t ss (insertMarble n zs)

parseInput :: String -> Maybe (Game, Game)
parseInput = maybe Nothing (Just . addGame) . (fst <$>) . runParser p
  where p = Game <$> (number <* string " players; last marble is worth ")
                 <*> (number <* string " points\n")
        addGame x = (x, updateTarget (* 100) x)

day09 :: String -> Showable
day09 s = let parseError = (pack "Unable to parse input")
              logicError = (pack "Error in game logic")
              input = parseInput s
              wrapResult = maybe logicError pack
              getResult (x, y) = pack (wrapResult (go x), wrapResult (go y))
          in maybe parseError getResult input
