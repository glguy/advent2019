{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 4 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/4>

-}
module Main (main) where

import           Advent
import           Data.List

main :: IO ()
main =
  do [(lo,hi)] <- getParsedLines 4 ((,) <$> number <* "-" <*> number)
     let nums = map runs $ filter nondecreasing $ map show [lo..hi::Int]
     print (count (any (> 1)) nums)
     print (count (elem 2)    nums)

runs :: Eq a => [a] -> [Int]
runs = map length . group

nondecreasing :: Ord a => [a] -> Bool
nondecreasing xs = and (zipWith (<=) xs (tail xs))
