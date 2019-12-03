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
import           Data.List (scanl', foldl1')
import           Data.Map (Map)
import qualified Data.Map as Map

parseSteps :: Parser [(Char, Int)]
parseSteps = liftA2 (,) anySingle number `sepBy` ","

main :: IO ()
main =
  do stepss <- getParsedLines 3 parseSteps
     let inter = foldl1' (Map.intersectionWith (+)) (map locations stepss)
     print (minimum (map (C.manhattan C.origin) (Map.keys inter)))
     print (minimum inter)

-- | Generate a map of the coordinates a path visits. Each coordinate is
-- indexed by the number of steps it took to get to that location.
locations :: [(Char, Int)] -> Map C.Coord Int
locations steps = Map.fromListWith min (zip (generatePath C.origin steps) [1..])

-- | Generate the list of coordinates visited by a list of steps.
generatePath :: C.Coord -> [(Char, Int)] -> [C.Coord]
generatePath c xs
  = tail -- drop starting point
  $ scanl' (\x f -> f x) c
  $ concatMap (\(d,n) -> replicate n (todir d)) xs

-- | Convert a direction letter to a coordinate update function.
todir :: Char -> C.Coord -> C.Coord
todir 'L' = C.left
todir 'R' = C.right
todir 'U' = C.above
todir 'D' = C.below
todir c   = error ("todir: bad direction " ++ show c)
