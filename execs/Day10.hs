{-# Language ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 10 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/10>

-}
module Main (main) where

import Advent
import Advent.Coord
import Data.List (maximumBy, sortOn, transpose)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord (comparing)
import Data.Ratio ((%))

main :: IO ()
main =
  do inp <- getInputLines 10
     let byAngles = findTheBase [c | (c,'#') <- coordLines inp]
     print (Map.size byAngles) -- part 1
     let C y x = spiralOrder byAngles !! 199
     print (x * 100 + y) -- part 2

collectBy :: Ord k => (a -> k) -> [a] -> Map k [a]
collectBy f xs = Map.fromListWith (++) [(f x, [x]) | x <- xs]

-- Given a list of asteroid locations, the other asteroids
-- arranged by their angle from the best base in order of
-- distance from that base.
findTheBase :: [Coord] -> Map Angle [Coord]
findTheBase world =
  maximumBy (comparing Map.size)
  [sortOn (manhattan i) <$> collectBy (angle . sub i) xs | (i,xs) <- pickOne world]

spiralOrder :: Map Angle [Coord] -> [Coord]
spiralOrder = concat . transpose . Map.elems

-- 'subtract' but for 'Coord'
sub :: Coord -> Coord -> Coord
sub (C y x) (C v u) = C (v-y) (u-x)

data Angle = Angle !Int !Rational -- quadrant and slope
  deriving (Show, Eq, Ord)

angle :: Coord -> Angle
angle (C y x)
  | x == 0, y == 0 = Angle 0 0
  | x >= 0, y < 0  = mk 1 x (-y)    -- upper right
  | y >= 0, x > 0  = mk 2 y x       -- lower right
  | x <= 0, y > 0  = mk 3 (-x) y    -- lower left
  | otherwise      = mk 4 (-y) (-x) -- upper left
  where
     mk i a b = Angle i (fromIntegral a % fromIntegral b)
