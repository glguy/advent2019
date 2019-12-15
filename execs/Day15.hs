{-|
Module      : Main
Description : Day 15 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/15>

-}
module Main (main) where

import           Advent
import           Advent.Coord
import           Advent.Intcode
import           Advent.Search
import           Data.List
import           Data.Set (Set)
import qualified Data.Set as Set


main :: IO ()
main =
  do [inp] <- getParsedLines 15 memoryParser

     let effect = run (new inp)
         outs = bfsOn (\(_,_,c,_) -> c) step1 (False, 0, origin, effect)
         Just (_, part1, oxygen, _) = find (\(x,_,_,_)->x) outs
     print part1

     let world = Set.fromList ([c | (_,_,c,_) <- outs])
         (part2, _) = last (bfsOn snd (search world) (0, oxygen))
     print part2

-- | Advance a robot one step, update its location
step1 :: (Bool, Int, Coord, Effect) -> [(Bool, Int, Coord, Effect)]
step1 (_, steps, here, Input f) =
  do (i,g) <- [(1,above),(2,below),(3,left),(4,right)]
     let here' = g here
     case f i of
       Output 1 e -> [(False, steps+1, here', e)]
       Output 2 e -> [(True , steps+1, here', e)]
       _          -> []
step1 _ = error "Expected input"

search :: Set Coord -> (Int, Coord) -> [(Int, Coord)]
search world (n, here) =
  do here' <- cardinal here
     [(n+1, here') | Set.member here' world]
