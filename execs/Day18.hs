{-# Language RecordWildCards #-}
{-|
Module      : Main
Description : Day 18 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/18>

This solution used roughly 40MB of RAM and runs
in 40 seconds on my laptop.

-}
module Main (main) where

import           Advent
import           Advent.Coord
import           Advent.Search
import           Data.Char
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Array.Unboxed

main :: IO ()
main =
  do inp <- coordLines <$> getInputLines 18
     let Just b = boundingBox (map fst inp)
         world1 = listArray b (map snd inp)
         start  = head [k | (k,'@') <- inp]
         keyN   = count (isLower . snd) inp

         done s = Set.size (akDoors (fst s)) == keyN
         select = snd . head . filter done

     print (select (allKeys world1 (Set.singleton start)))

     let world2 = world1 // [(c,'#') | c <- start : cardinal start]
         start2 = Set.fromList [ f (g start) | f <- [above, below]
                                             , g <- [left , right] ]

     print (select (allKeys world2 start2))

------------------------------------------------------------------------
-- Search that moves robots from key to key
------------------------------------------------------------------------

data AllKeys = AllKeys
  { akLocation :: !(Set Coord) -- ^ robot locations
  , akDoors    :: !(Set Char)  -- ^ opened doors
  }
  deriving (Ord, Eq, Show)

allKeys ::
  UArray Coord Char {- ^ world map               -} ->
  Set Coord         {- ^ robot locations         -} ->
  [(AllKeys, Int)]  {- ^ search states and costs -}
allKeys world start = astar stepAK (AllKeys start Set.empty)
  where
    summaries = travelSummary world (Set.toList start)

    stepAK AllKeys{..} =
      [ (AllKeys (Set.insert loc (Set.delete who akLocation))
                 doors'
        , cost {- cost -}, 0)
        | who <- Set.toList akLocation
        , (loc, key, doors, cost) <- Map.findWithDefault undefined who summaries
        , Set.notMember key akDoors
        , doors `Set.isSubsetOf` akDoors
        , let doors' = Set.insert key akDoors
        ]

------------------------------------------------------------------------
-- Search that finds shortest distances to the remaining keys
------------------------------------------------------------------------

data Shortest = Shortest
  { sSteps    :: !Int   -- ^ steps taken
  , sLocation :: !Coord -- ^ current location
  , sDoors    :: !(Set Char)
  }
  deriving Show

travelSummary :: UArray Coord Char -> [Coord] -> Map Coord [(Coord, Char, Set Char, Int)]
travelSummary world starts =
  Map.fromList $
    [(start, travelFrom start) | start <- starts] ++
    [(start, travelFrom start) | (start, startKey) <- assocs world , isLower startKey]
  where
    travelFrom start =
      [ (sLocation s, toUpper k, sDoors s, sSteps s)
         | s <- bfsOn sLocation step1 (Shortest 0 start Set.empty)
         , let k = world ! sLocation s
         , isLower k ]

    step1 Shortest{..} =
      [ Shortest (sSteps+1) here doors'

         -- Take a step but don't stop on a wall
         | here <- cardinal sLocation
         , let cell = world ! here
         , cell /= '#'
         , let doors' | 'A' <= cell, cell <= 'Z' = Set.insert cell sDoors
                      | otherwise = sDoors
         ]
