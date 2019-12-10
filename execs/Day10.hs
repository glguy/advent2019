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
import Data.Ord

main :: IO ()
main =
  do inp <- getParsedLines 10 (many (True <$ "#" <|> False <$ "."))

     let m :: Set (Int,Int)
         m = Set.fromList
                [ ((c,r)) | (r,row) <- zip [0..] inp, (c,True) <- zip [0..] row ]

     let (base, vis) = maximumBy (comparing snd) $ [ (i, count (visible m i) m) | i <-  toList m ]
     print vis
     let (x,y) = part2 base (Set.delete base m) !! 199
     print (x * 100 + y)


part2 :: (Int,Int) -> Set (Int,Int) -> [(Int,Int)]
part2 base m
  | Set.null m = []
  | otherwise  = these ++ part2 base (Set.difference m (Set.fromList these))
  where
    these = filter (visible m base) (sortOn (angle . sub base) ((toList m)))

sub :: (Int,Int) -> (Int,Int) -> (Int,Int)
sub (x,y) (u,v) = (u-x, v-y)

angle :: (Int,Int) -> Double
angle (x,y) = angle' (x,-y)

angle' :: (Int,Int) -> Double
angle' (x,y)
  | x == 0, y >= 0 = 0
  | x >  0, y >= 0 = pi/2 - atan (fromIntegral y / fromIntegral x)
  | x >  0, y <  0 = pi/2 - atan (fromIntegral y / fromIntegral x)
  | x == 0, y <  0 = pi
  | x <  0, y <  0 = 3*pi/2 - atan (fromIntegral y / fromIntegral x)
  | otherwise      = 3*pi/2 - atan (fromIntegral y / fromIntegral x)

visible :: Set (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
visible _ x y | x == y = False
visible ast (x,y) (u,v) =
  not $
  any (`Set.member` ast)
      [ (u + stepx * i, v + stepy * i)   | i <- [1 .. n-1] ]
  where
    dx = x - u
    dy = y - v
    step = gcd dx dy
    n | dx /= 0   = dx `div` stepx
      | otherwise = dy `div` stepy

    stepx = dx `div` step
    stepy = dy `div` step
