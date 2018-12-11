module Day10 (day10) where

import Data.List (groupBy, sortBy)

import Parser
import Types

data Position = Position Int Int
  deriving Show
data Velocity = Velocity Int Int
  deriving Show
data Point = Point Position Velocity
  deriving Show

makeParser :: String -> (Int -> Int -> a) -> Parser a
makeParser s f = f <$>
  (string (s ++ "=<") *> spaces *> number)
  <*> (char ',' *> spaces *> number <* char '>')

position :: Parser Position
position = makeParser "position" Position

velocity :: Parser Velocity
velocity = makeParser "velocity" Velocity

point :: Parser Point
point = Point <$> (position <* spaces) <*> velocity

distance :: Point -> Point -> Int
distance (Point (Position x1 y1) _) (Point (Position x2 y2) _) =
  (abs $ x1 - x2) + (abs $ y1 - y2)

neighborDistance :: ([Int] -> Int) -> [Point] -> [Int]
neighborDistance g ps = foldr (f ps) [] ps
  where f ps q qs = let xs = filter (/= 0) $ map (distance q) ps
                    in if null xs then qs else (g xs) : qs

maxDistance :: [Point] -> Int
maxDistance = maximum . neighborDistance maximum

updatePoint :: Point -> Point
updatePoint (Point (Position x y) (Velocity u v)) =
  Point (Position (x+u) (y+v)) (Velocity u v)

updatePoints :: [Point] -> [Point]
updatePoints = map updatePoint

pointToPair :: Point -> (Int, Int)
pointToPair (Point (Position x y) _) = (x, y)

pointsToPairs :: [Point] -> [(Int, Int)]
pointsToPairs = map pointToPair

normalize :: [(Int, Int)] -> [(Int, Int)]
normalize ps = let xMin = minimum $ map fst ps
                   yMin = minimum $ map snd ps
                   f (x, y) = (x-xMin, y-yMin)
               in map f ps

xsToString :: [Int] -> String
xsToString xs = let m = maximum xs
                    getChar n = if n `elem` xs then '#' else ' '
                in [getChar n | n <- [0..m]]

stringify :: [(Int, Int)] -> String
stringify ps = let srt (a, b) (c, d) = compare b d <> compare a c
                   grp (_, b) (_, d) = b == d
                   xs = map (map fst) $ groupBy grp $ sortBy srt ps
               in unlines $ map xsToString xs

go :: [Point] -> (String, Int)
go ps = let ps' = iterate updatePoints ps
            ds = map maxDistance ps'
            dps = zip ds ps'
            decreasing ((a, _), (b, _)) = a >= b
            pairs = zip dps (tail dps)
            a = stringify $ normalize $ pointsToPairs $ snd $ fst $ head $ dropWhile decreasing pairs
            b = length $ takeWhile decreasing pairs
        in (a, b)

parseInput :: String -> Maybe [Point]
parseInput = sequence . map ((fst <$>) . runParser point) . lines

day10 :: String -> Showable
day10 = maybe (pack "Error parsing input") (pack . go) . parseInput
