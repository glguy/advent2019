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

import Advent         (getParsedLines)
import Advent.Intcode (run, new, effectList, memoryParser)
import Data.Function  (fix)
import Data.List      (permutations)

main :: IO ()
main =
  do [pgm] <- getParsedLines 7 memoryParser

     -- evaluate the program as a function from inputs to outputs
     let eff :: [Int] -> [Int]
         eff = effectList (run 0 (new pgm))

     -- given a list of phase settings compute the thrust outputs
     let startup :: [Int] -> [Int]
         startup = tieknot . map (eff <<)

     print $ maximum $ head . startup <$> permutations [0..4]
     print $ maximum $ last . startup <$> permutations [5..9]

-- | Given a list of functions on lists of integers,
-- take the outputs from each element and pass it through
-- as the inputs to the next function. The results from the
-- last function are fed back into the first function.
-- The first function is started with a leading zero.
tieknot :: [ [Int] -> [Int] ] -> [Int]
tieknot fs = fix (compose fs << 0)

-- | Compose a list of functions together
compose :: [a -> a] -> a -> a
compose = foldr (.) id

-- | Given a function that takes a list argument
-- return a new function that prefixes an extra
-- element onto the eventual list argument.
(<<) :: ([a] -> b) -> a -> [a] -> b
(f << x) xs = f (x:xs)
