{-|
Module      : Main
Description : Day 25 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/25>

-}
module Main (main) where

import Advent
import Advent.Intcode
import Data.Char


main :: IO ()
main =
  do [inp] <- getParsedLines 25 memoryParser
     putStrLn $ map chr $ intCodeToList inp $ map ord $ unlines script

script :: [String]
script =
  [north, north, take_ "space heater",
   east, take_ "semiconductor",
   west, south, south, east, take_ "ornament",
   south, take_ "festive hat",
   north, west, west, north, west]
  where
    north = "north"
    south = "south"
    east  = "east"
    west  = "west"
    take_ x = "take " ++ x
