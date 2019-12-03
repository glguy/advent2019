{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 3 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/3>

-}
module Main (main) where

import           Advent (Parser, sepBy, getParsedLines, anySingle, number)
import qualified Advent.Coord as C
import           Control.Applicative (liftA2)
import           Data.Map (Map)
import qualified Data.Map as Map

parseSteps :: Parser [(Char, Int)]
parseSteps = liftA2 (,) anySingle number `sepBy` ","

main :: IO ()
main =
  do [inp1,inp2] <- getParsedLines 3 parseSteps
     let inter = Map.intersectionWith (+)
                   (locations inp1)
                   (locations inp2)
     print (minimum (map (C.manhattan C.origin) (Map.keys inter)))
     print (minimum inter)

-- | Generate a map of the coordinates a path visits. Each coordinate is
-- indexed by the number of steps it took to get to that location.
locations :: [(Char, Int)] -> Map C.Coord Int
locations steps = Map.fromList (zip (generatePath C.origin steps) [1..])

-- | Generate the list of coordinates visited by a list of steps.
generatePath :: C.Coord -> [(Char, Int)] -> [C.Coord]
generatePath _ [] = []
generatePath c ((d,n):xs) = drop 1 ys ++ generatePath (last ys) xs
  where
    ys = take (n+1) (iterate (todir d) c)

-- | Convert a direction letter to a coordinate update function.
todir :: Char -> C.Coord -> C.Coord
todir 'L' = C.left
todir 'R' = C.right
todir 'U' = C.above
todir 'D' = C.below
todir c   = error ("todir: bad direction " ++ show c)
