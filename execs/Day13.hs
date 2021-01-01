{-# Language ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 13 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/13>

-}
module Main (main) where

import Advent (getIntcodeInput)
import Data.Map (Map)
import Data.Map qualified as Map
import Intcode (Effect(..), new, run, set)

main :: IO ()
main =
  do mach <- new <$> getIntcodeInput 13
     print (part1       (run          mach ))
     print (part2 0 0 0 (run (set 0 2 mach)))

part1 :: Effect -> Int
part1 = Map.size . Map.filter (2==) . getImage Map.empty

getImage :: Map (Int, Int) Int -> Effect -> Map (Int, Int) Int
getImage m (Output x (Output y (Output t e))) = getImage (Map.insert (x,y) t m) e
getImage m _                                  = m

part2 :: Int -> Int -> Int -> Effect -> Int
part2 ball paddle score effect =
  case effect of
    Output x (Output y (Output t effect'))
      | t == 4            -> part2 x    paddle score effect'
      | t == 3            -> part2 ball x      score effect'
      | x == (-1), y == 0 -> part2 ball paddle t     effect'
      | otherwise         -> part2 ball paddle score effect'
    Input f               -> part2 ball paddle score (f (signum (ball - paddle)))
    _                     -> score
