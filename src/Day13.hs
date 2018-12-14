module Day13 (day13) where

import Control.Applicative hiding (many)
import Data.List (sort)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

import Parser
import Types

data Path = NS | EW | RP | LP | Intersection | Empty

data Cart = NCart | SCart | ECart | WCart
  deriving Eq

data TrackElement = TrackPath Path | TrackCart Cart

data Turn = LTurn | Straight | RTurn
  deriving (Eq, Show)

data Coord = Coord Int Int
  deriving (Show, Eq)

data CartNav = CartNav Cart Coord [Turn]
  deriving Eq

data Track = Track (Map Coord Path) [CartNav]
  deriving Show

data TickResult = Crash Coord | TickResult Track

instance Show Path where
  show NS = "|"
  show EW = "-"
  show RP = "/"
  show LP = "\\"
  show Intersection = "+"
  show Empty = " "

instance Show Cart where
  show NCart = "^"
  show SCart = "v"
  show ECart = ">"
  show WCart = "<"

instance Show TrackElement where
  show (TrackPath p) = show p
  show (TrackCart c) = show c

instance Show CartNav where
  show (CartNav cart coord turns) =
    "CartNav " ++ (show cart) ++ " " ++ show coord ++ " " ++ show (take 3 turns)

instance Ord Coord where
  compare (Coord x y) (Coord a b) = compare y b <> compare x a

instance Ord CartNav where
  compare (CartNav _ a _) (CartNav _ b _) = compare a b

cartNavCoord :: CartNav -> Coord
cartNavCoord (CartNav _ c _) = c

parsePath :: Parser Path
parsePath = const NS <$> char '|'
        <|> const EW <$> char '-'
        <|> const RP <$> char '/'
        <|> const LP <$> char '\\'
        <|> const Intersection <$> char '+'
        <|> const Empty <$> char ' '

parseCart :: Parser Cart
parseCart = const NCart <$> char '^'
        <|> const SCart <$> char 'v'
        <|> const ECart <$> char '>'
        <|> const WCart <$> char '<'

parseTrackElement :: Parser TrackElement
parseTrackElement = (TrackPath <$> parsePath)
                <|> (TrackCart <$> parseCart)

size :: [[a]] -> (Int, Int)
size xss = (maximum $ map length xss, length xss)

coords :: (Int, Int) -> [Coord]
coords (x, y) = [Coord x' y' | y' <- [0..y-1], x' <- [0..x-1]]

north :: Coord -> Coord
north (Coord x y) = Coord x (y-1)

south :: Coord -> Coord
south (Coord x y) = Coord x (y+1)

east :: Coord -> Coord
east (Coord x y) = Coord (x+1) y

west :: Coord -> Coord
west (Coord x y) = Coord (x-1) y

intersection :: CartNav -> CartNav
intersection c@(CartNav _ _ []) = c
intersection (CartNav NCart coord (LTurn:xs)) = CartNav WCart (west coord) xs
intersection (CartNav NCart coord (Straight:xs)) = CartNav NCart (north coord) xs
intersection (CartNav NCart coord (RTurn:xs)) = CartNav ECart (east coord) xs
intersection (CartNav SCart coord (LTurn:xs)) = CartNav ECart (east coord) xs
intersection (CartNav SCart coord (Straight:xs)) = CartNav SCart (south coord) xs
intersection (CartNav SCart coord (RTurn:xs)) = CartNav WCart (west coord) xs
intersection (CartNav ECart coord (LTurn:xs)) = CartNav NCart (north coord) xs
intersection (CartNav ECart coord (Straight:xs)) = CartNav ECart (east coord) xs
intersection (CartNav ECart coord (RTurn:xs)) = CartNav SCart (south coord) xs
intersection (CartNav WCart coord (LTurn:xs)) = CartNav SCart (south coord) xs
intersection (CartNav WCart coord (Straight:xs)) = CartNav WCart (west coord) xs
intersection (CartNav WCart coord (RTurn:xs)) = CartNav NCart (north coord) xs

newLocation :: Map Coord Path -> CartNav -> CartNav
newLocation m n@(CartNav cart coord turns) =
  case (Map.findWithDefault Empty coord m, cart) of
    (NS, NCart) -> CartNav NCart (north coord) turns
    (NS, SCart) -> CartNav SCart (south coord) turns
    (EW, ECart) -> CartNav ECart (east coord) turns
    (EW, WCart) -> CartNav WCart (west coord) turns
    (RP, NCart) -> CartNav ECart (east coord) turns
    (RP, SCart) -> CartNav WCart (west coord) turns
    (RP, ECart) -> CartNav NCart (north coord) turns
    (RP, WCart) -> CartNav SCart (south coord) turns
    (LP, NCart) -> CartNav WCart (west coord) turns
    (LP, SCart) -> CartNav ECart (east coord) turns
    (LP, ECart) -> CartNav SCart (south coord) turns
    (LP, WCart) -> CartNav NCart (north coord) turns
    (Intersection, _) -> intersection n
    (_, _) -> n

findAccident :: [(CartNav, [CartNav])] -> Maybe Coord
findAccident xs = maybe (postTickCrash xs) Just $ midTickCrash xs
  where midTickCrash = foldl f Nothing
        postTickCrash = findCrash . sort . map (cartNavCoord . fst)
        findCrash (x:y:zs) = if x == y then Just x else findCrash (y:zs)
        findCrash _ = Nothing
        f Nothing (c, cs) =
          let c' = cartNavCoord c
              cs' = map cartNavCoord cs
          in if c' `elem` cs' then Just c' else Nothing
        f jc _ = jc

notCrash :: TickResult -> Bool
notCrash (TickResult _) = True
notCrash _ = False

showResult :: TickResult -> Showable
showResult (TickResult x) = pack x
showResult (Crash x) = pack x

tick :: TickResult -> TickResult
tick c@(Crash _) = c
tick (TickResult (Track m xs)) = let moves = tick' m (sort xs)
                                     track = Track m (map fst moves)
                                 in maybe (TickResult track) Crash (findAccident moves)
  where tick' _ [] = []
        tick' m (x:xs) = (newLocation m x, xs) : (tick' m xs)

initTrack :: [(Coord, TrackElement)] -> Track
initTrack = foldr f (Track Map.empty [])
  where turns = cycle [LTurn, Straight, RTurn]
        f (c, (TrackPath p)) (Track m xs) = Track (Map.insert c p m) xs
        f (c, (TrackCart NCart)) (Track m xs) = Track (Map.insert c NS m) (CartNav NCart c turns : xs)
        f (c, (TrackCart SCart)) (Track m xs) = Track (Map.insert c NS m) (CartNav SCart c turns : xs)
        f (c, (TrackCart ECart)) (Track m xs) = Track (Map.insert c EW m) (CartNav ECart c turns : xs)
        f (c, (TrackCart WCart)) (Track m xs) = Track (Map.insert c EW m) (CartNav WCart c turns : xs)

go :: [String] -> Showable
go xs = let cs = coords (size xs)
            ts = runParser (many parseTrackElement) (concat xs)
            ts' = maybe [] fst ts
            ticks = iterate tick $ TickResult $ initTrack (zip cs ts')
        in showResult $ head $ dropWhile notCrash ticks

day13 :: String -> Showable
day13 = go . lines
