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
import           Control.Applicative (liftA2)
import           Data.Foldable (asum)
import           Data.List (scanl', foldl1')
import           Data.Map (Map)
import qualified Data.Map as Map

-- $setup
-- >>> let parse = Data.Either.fromRight undefined . Advent.parseLines parseSteps . unlines

data Motion = Motion Direction Int
  deriving Show

data Direction = U | L | D | R
  deriving Show

type Coord = (Int,Int)

parseDirection :: Parser Direction
parseDirection = asum [U <$ "U", D <$ "D", R <$ "R", L <$ "L"]

parseSteps :: Parser [Motion]
parseSteps = liftA2 Motion parseDirection number `sepBy` ","

main :: IO ()
main =
  do (p1,p2) <- answers <$> getParsedLines 3 parseSteps
     print p1
     print p2

-- | Pair-wise addition of coordinates.
--
-- >>> addCoord (1,2) (3,4)
-- (4,6)
addCoord :: Coord -> Coord -> Coord
addCoord (x,y) (dx,dy) = (x+dx, y+dy)

-- | Manhattan distance from origin for a given coordinate
--
-- >>> manhattan (3,4)
-- 7
-- >>> manhattan (-3,-5)
-- 8
manhattan :: Coord -> Int
manhattan (x,y) = abs x + abs y

-- | Given the input file parsed as lists of lists of motions, compute the
-- nearest distance to origin and minimum sum steps to intersection.
--
-- >>> let check = answers . parse
-- >>> check ["R8,U5,L5,D3","U7,R6,D4,L4"]
-- (6,30)
-- >>> check ["R75,D30,R83,U83,L12,D49,R71,U7,L72","U62,R66,U55,R34,D71,R55,D58,R83"]
-- (159,610)
-- >>> check ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51","U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]
-- (135,410)
answers :: [[Motion]] -> (Int, Int)
answers xs = (nearestDistanceToOrigin intersections, minimum intersections)
  where
    intersections = pathIntersections xs

-- | Computes the map of path intersections, compute the shortest
-- distance of an intersection to the origin.
nearestDistanceToOrigin :: Map Coord a -> Int
nearestDistanceToOrigin = minimum . map manhattan . Map.keys

-- | Given a list of paths compute a map of locations that have intersections
-- among all of the paths. The value at each location is the sum of the
-- number of steps taken along each of the paths to get to that point.
--
-- >>> let check = pathIntersections . parse
-- >>> check ["R8,U5,L5,D3","U7,R6,D4,L4"]
-- fromList [((3,-3),40),((6,-5),30)]
pathIntersections :: [[Motion]] -> Map Coord Int
pathIntersections = foldl1' (Map.intersectionWith (+)) . map locations

-- | Generate a map of the coordinates a path visits. Each coordinate is
-- indexed by the number of steps it took to get to that location.
--
-- >>> locations [Motion D 2, Motion R 1]
-- fromList [((0,1),1),((0,2),2),((1,2),3)]
locations :: [Motion] -> Map Coord Int
locations steps = Map.fromListWith min (zip (generatePath steps) [1..])

-- | Generate the list of coordinates visited by a list of steps.
--
-- >>> generatePath [Motion D 2, Motion R 1]
-- [(0,1),(0,2),(1,2)]
generatePath :: [Motion] -> [Coord]
generatePath
  = tail -- drop starting point
  . scanl' addCoord (0,0)
  . concatMap (\(Motion d n) -> replicate n (toUnitVector d))

-- | Convert a direction letter unit vector in the given direction.
toUnitVector :: Direction -> Coord
toUnitVector U = ( 0, -1)
toUnitVector D = ( 0,  1)
toUnitVector L = (-1,  0)
toUnitVector R = ( 1,  0)
