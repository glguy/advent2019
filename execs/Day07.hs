{-# Language RecordWildCards, OverloadedStrings #-}
{-|
Module      : Main
Description : Day 5 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/7>

-}
module Main (main) where

import Advent         (compose, getParsedLines)
import Advent.Intcode (intCodeToList, memoryParser)
import Data.Function  (fix)
import Data.List      (permutations)

main :: IO ()
main =
  do [pgm] <- map intCodeToList <$> getParsedLines 7 memoryParser
     print (part1 pgm)
     print (part2 pgm)

-- |
--
-- >>> part1 (intCodeToList [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0])
-- 43210
-- >>> part1 (intCodeToList [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0])
-- 54321
-- >>> part1 (intCodeToList [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0])
-- 65210
part1 :: ([Int] -> [Int]) -> Int
part1 pgm = maximum (head . startup pgm <$> permutations [0..4])

-- |
--
-- >>> part2 (intCodeToList [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5])
-- 139629729
-- >>> :{
-- >>> part2 (intCodeToList [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
-- >>>                       -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
-- >>>                       53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10])
-- >>> :}
-- 18216
part2 :: ([Int] -> [Int]) -> Int
part2 pgm = maximum (last . startup pgm <$> permutations [5..9])

-- given a list of phase settings compute the thrust outputs
startup ::
  ([Int] -> [Int]) {- ^ single controller -} ->
  [Int]            {- ^ phase settings    -} ->
  [Int]            {- ^ thruster output   -}
startup ctrl phases = tieknot [ ctrl << p | p <- phases ]
  where
    tieknot fs = fix (compose fs << 0)

    (f << x) xs = f (x:xs)
