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

-- | Parses two numbers separated by a dash.
rangeParser :: Parser (Int, Int)
rangeParser = (,) <$> number <* "-" <*> number

main :: IO ()
main =
  do [(lo,hi)] <- getParsedLines 4 rangeParser
     let nums = map runs $ filter nondecreasing $ map show [lo..hi]
     print (count (any (> 1)) nums)
     print (count (elem 2   ) nums)

-- | Return a list of the lengths of consecutive elements in a list.
--
-- >>> runs [1,2,3]
-- [1,1,1]
-- >>> runs [1,1,1]
-- [3]
-- >>> runs [1,1,2,2,2,1,1]
-- [2,3,2]
-- >>> runs []
-- []
runs :: Eq a => [a] -> [Int]
runs = map length . group

-- | Predicate for non-decreasing lists.
--
-- >>> nondecreasing []
-- True
-- >>> nondecreasing [1,1,2,3]
-- True
-- >>> nondecreasing [3,3,2]
-- False
nondecreasing :: Ord a => [a] -> Bool
nondecreasing xs = and (zipWith (<=) xs (tail xs))
