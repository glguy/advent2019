{-|
Module      : Main
Description : Day 21 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/21>

-}
module Main (main) where

import           Advent         (getParsedLines)
import           Advent.Intcode (intCodeToList, memoryParser)
import           Data.Char      (ord)

main :: IO ()
main =
  do [inp] <- getParsedLines 21 memoryParser
     let letsGo = print . last . intCodeToList inp . map ord
     letsGo part1
     letsGo part2

-- !(A ∧ C) ∧ D   # test cases didn't need B to be checked
part1 :: String
part1 =
  unlines
    [ "OR  A J",
      "AND C J",
      "NOT J J",
      "AND D J",
      "WALK" ]

-- !(A ∧ B ∧ C) ∧ D ∧ (E ∨ H)
part2 :: String
part2 =
  unlines
    [ "OR  A J",
      "AND B J",
      "AND C J",
      "NOT J J",
      "AND D J",
      "OR  E T",
      "OR  H T",
      "AND T J",
      "RUN" ]
