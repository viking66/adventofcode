module Day03 (day03) where

import Types
day03 :: String -> Showable
day03 _ = let x = (0, 0) :: (Int, Int)
          in pack x
