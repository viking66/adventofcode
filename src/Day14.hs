module Day14 (day14) where

import Data.Bifunctor (bimap)
import Data.Char (digitToInt)

import Parser hiding (digits)
import Types

data ZipList a = ZipList [a] a [a] [a]

instance (Show a) => Show (ZipList a) where
  show = show . toList

data Recipes = Recipes (ZipList Int) (ZipList Int) Int

toList :: ZipList a -> [a]
toList (ZipList ws x yz zs) = reverse ws ++ (x:yz) ++ reverse zs

recipesToList :: Recipes -> [Int]
recipesToList (Recipes z _ _) = toList z

focus :: ZipList a -> a
focus (ZipList _ a _ _) = a

moveRight :: ZipList a -> ZipList a
moveRight (ZipList ws x (y:ys) zs) = ZipList (x:ws) y ys zs
moveRight (ZipList ws x [] []) = let (y:ys) = reverse (x:ws)
                                 in ZipList [] y ys []
moveRight (ZipList ws x [] zs) = let (y:ys) = reverse zs
                                 in ZipList (x:ws) y ys []

moveRightN :: Int -> ZipList a -> ZipList a
moveRightN 0 z = z
moveRightN n z = moveRightN (n-1) (moveRight z)

append :: a -> ZipList a -> ZipList a
append a (ZipList ws x ys zs) = ZipList ws x ys (a:zs)

currentRecipes :: Recipes -> (Int, Int)
currentRecipes (Recipes y z _) = (focus y, focus z)

addRecipe :: Int -> Recipes -> Recipes
addRecipe n (Recipes y z m) = Recipes (append n y) (append n z) (succ m)

digits :: Int -> [Int]
digits = map digitToInt . show

updateRecipes :: Recipes -> Recipes
updateRecipes r = foldl (flip addRecipe) r $ digits $ uncurry (+) $ currentRecipes r

rotateRecipes :: Recipes -> (Int, Int) -> Recipes
rotateRecipes (Recipes y z n) (a, b) = Recipes (moveRightN a y) (moveRightN b z) n

nextRecipes :: Recipes -> Recipes
nextRecipes r = rotateRecipes r $ bimap succ succ $ currentRecipes r

recipesLength :: Recipes -> Int
recipesLength (Recipes _ _ n) = n

initRecipes :: Recipes
initRecipes = Recipes (ZipList [] 3 [7] []) (ZipList [3] 7 [] []) 2

go1 :: Int -> String
go1 n = concatMap show $ take 10 $ drop n $ recipesToList $ go' n initRecipes
  where go' n r = if recipesLength r >= (n + 10)
                  then r
                  else go' n (nextRecipes $ updateRecipes r)

go :: Int -> String
go n = go1 n

parseInput :: String -> Maybe Int
parseInput = (fst <$>) . runParser number . filter (/= '\n')

day14 :: String -> Showable
day14 = maybe (pack "Failed to parse input") (pack . go) . parseInput
