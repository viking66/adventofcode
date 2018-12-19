module Day18 (day18) where

import Control.Applicative hiding (many)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import Parser
import Types

data Coord = Coord Int Int
  deriving (Show, Eq)

data Acre = Open | Tree | Lumberyard
  deriving (Eq, Ord, Show)

data Landscape = Landscape Acre
  deriving (Eq, Ord, Show)

type Acres = Map Coord Landscape

instance Ord Coord where
  compare (Coord x y) (Coord a b) = compare y b <> compare x a

landscapeParser :: Parser Landscape
landscapeParser = Landscape <$> acreParser
  where openParser = const Open <$> char '.'
        treeParser = const Tree <$> char '|'
        lumberyardParser = const Lumberyard <$> char '#'
        acreParser = openParser <|> treeParser <|> lumberyardParser

size :: [[a]] -> (Int, Int)
size xss = (maximum $ map length xss, length xss)

coords :: (Int, Int) -> [Coord]
coords (x, y) = [Coord x' y' | y' <- [0..y-1], x' <- [0..x-1]]

isTree :: Landscape -> Bool
isTree (Landscape Tree) = True
isTree _ = False

isLumberyard :: Landscape -> Bool
isLumberyard (Landscape Lumberyard) = True
isLumberyard _ = False

neighbors :: Acres -> Coord -> [Landscape]
neighbors a (Coord x y) = maybe [] id $ sequence
                                      $ filter isJust
                                      $ map (flip Map.lookup a) cs
  where cs = tail [Coord (x+x') (y+y') | x' <- [0,-1,1], y' <- [0,-1,1]]
        isJust (Just _) = True
        isJust _ = False

updateAcre :: Acres -> Coord -> Landscape -> Landscape
updateAcre as c l@(Landscape Open) =
  if (>=3) $ length $ filter isTree $ neighbors as c
  then Landscape Tree
  else l
updateAcre as c l@(Landscape Tree) =
  if (>=3) $ length $ filter isLumberyard $ neighbors as c
  then Landscape Lumberyard
  else l
updateAcre as c l@(Landscape Lumberyard) =
  let ns = neighbors as c
      ls = filter isLumberyard ns
      ts = filter isTree ns
  in if null ls || null ts
  then Landscape Open
  else l

updateAcres :: Acres -> Acres
updateAcres a = Map.mapWithKey (updateAcre a) a

setup :: String -> Acres
setup xs = let cs = coords $ size $ lines xs
               xs' = filter (/= '\n') xs
               ls = maybe [] fst $ runParser (many landscapeParser) xs'
           in Map.fromList $ zip cs ls

go1 :: Acres -> Int
go1 a = let ls = Map.elems $ iterate updateAcres a !! 10
            t = length $ filter isTree ls
            l = length $ filter isLumberyard ls
        in l * t

-- After some iterations the values will start to cycle.
-- Find where the cycle begins then use that to calulate
-- the final result
go2 :: Acres -> [Int]
go2 = take 1000 . map f . iterate updateAcres
  where f a = let ls = Map.elems a
                  t = length $ filter isTree ls
                  l = length $ filter isLumberyard ls
              in l * t

day18 :: String -> Showable
day18 s = let a = setup s
          in pack (go1 a, go2 a)
