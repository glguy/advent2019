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
import           Control.Applicative
import           Data.List
import           Data.Foldable
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Ord (comparing)

main :: IO ()
main =
  do inp <- getParsedLines 10 (many (True <$ "#" <|> False <$ "."))

     let m :: Set (Int,Int)
         m = Set.fromList
                [ ((x,y)) | (y,row) <- zip [0..] inp, (x,True) <- zip [0..] row ]

     let (base, vis) = maximumBy (comparing snd) [ (i, count (visible m i) m) | i <-  toList m ]
     print vis
     let (x,y) = part2 base (Set.delete base m) !! 199
     print (x * 100 + y)

part2 :: (Int,Int) -> Set (Int,Int) -> [(Int,Int)]
part2 base m
  | Set.null m = []
  | otherwise  = these ++ part2 base (Set.difference m (Set.fromList these))
  where
    these = filter (visible m base) (sortOn (angle . sub base) (toList m))

sub :: (Int,Int) -> (Int,Int) -> (Int,Int)
sub (x,y) (u,v) = (u-x, v-y)

-- |
--
-- >>> angle (0,-1) < angle (1,0)
-- True
-- >>> angle (1,0) < angle (0,1)
-- True
-- >>> angle (0,1) < angle (-1,0)
-- True
angle :: (Int,Int) -> Double
angle (x,y) = atan2 (fromIntegral y) (fromIntegral x)

visible :: Set (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
visible _ x y | x == y = False
visible ast (x,y) (u,v) =
  and [ Set.notMember (u + stepx * i, v + stepy * i) ast | i <- [1 .. steps-1] ]
  where
    dx = x - u
    dy = y - v
    steps = gcd dx dy

    stepx = dx `div` steps
    stepy = dy `div` steps
