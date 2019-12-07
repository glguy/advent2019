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

import Advent (getParsedLines)
import Advent.Intcode
import Data.List

main :: IO ()
main =
  do [pgm] <- getParsedLines 7 memoryParser

     let eff = effectList (run 0 (new pgm))
         startup = part2 . map (\i -> eff . (i:))

     print $ maximum $ head . startup <$> permutations [0..4]
     print $ maximum $ last . startup <$> permutations [5..9]

part2 :: [ [Int] -> [Int] ] -> [Int]
part2 xs = res
  where
    res = foldr id (0:res) xs
