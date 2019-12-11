{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 10 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/10>

-}
module Main (main) where

import           Advent
import           Advent.Coord
import           Control.Applicative
import           Data.List
import           Data.Foldable
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Ord (comparing)

main :: IO ()
main =
  do inp <- getParsedLines 10 (many (True <$ "#" <|> False <$ "."))

     let m :: Set Coord
         m = Set.fromList
                [ (C y x) | (y,row) <- zip [0..] inp, (x,True) <- zip [0..] row ]

     let (base, vis) = maximumBy (comparing snd) [ (i, count (visible m i) m) | i <-  toList m ]
     print vis
     let C y x = part2 base (Set.delete base m) !! 199
     print (x * 100 + y)

part2 :: Coord -> Set Coord -> [Coord]
part2 base m
  | Set.null m = []
  | otherwise  = these ++ part2 base (Set.difference m (Set.fromList these))
  where
    these = filter (visible m base) (sortOn (angle . sub base) (toList m))

sub :: Coord -> Coord -> Coord
sub (C y x) (C v u) = C (v-y) (u-x)

-- | Angle measure that sorts clockwise starting from 12 o'clock
--
-- >>> let ordered = [C (-1) 0,C (-1) 1,C 0 1,C 1 1,C 1 0,C 1 (-1),C 0 (-1),C (-1) (-1)]
-- >>> sortOn angle ordered == ordered
-- True
angle :: Coord -> Double
angle (C y x) = - atan2 (fromIntegral x) (fromIntegral y)

visible :: Set Coord -> Coord -> Coord -> Bool
visible _ x y | x == y = False
visible ast (C y x) (C v u) =
  and [ Set.notMember (C (v + stepy * i) (u + stepx * i)) ast | i <- [1 .. steps-1] ]
  where
    dx = x - u
    dy = y - v
    steps = gcd dx dy

    stepx = dx `div` steps
    stepy = dy `div` steps
