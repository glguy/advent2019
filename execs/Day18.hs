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
import           Data.List
import           Data.Char
import           Data.Set (Set)
import qualified Data.Set as Set
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

coordLines :: [String] -> [(Coord, Char)]
coordLines rows = [(C y x, z) | (y,row) <- zip [0..] rows, (x,z) <- zip [0..] row]

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
    stepAK AllKeys{..} =
      [ (AllKeys (Set.insert loc (Set.delete who akLocation))
                 (Set.insert door akDoors)
        , steps {- cost -}, 0 {- heuristic -} )
        | who <- Set.toList akLocation
        , (steps, loc, door) <- nextKeys world akDoors who
        ]

data FindKey = FindKey
  { fkSteps    :: !Int   -- ^ steps taken
  , fkLocation :: !Coord -- ^ current location
  }
  deriving Show

------------------------------------------------------------------------
-- Search that finds shortest distances to the remaining keys
------------------------------------------------------------------------

nextKeys ::
  UArray Coord Char    {- ^ world map                            -} ->
  Set Char             {- ^ open doors                           -} ->
  Coord                {- ^ starting point                       -} ->
  [(Int, Coord, Char)] {- ^ list of steps, location, door opened -}
nextKeys world found start =
  [ (fkSteps, fkLocation, door)
      | FindKey{..} <- bfsOn fkLocation stepFK (FindKey 0 start)
      , let key = world ! fkLocation
      , isLower key              -- only stop on keys
      , let door = toUpper key
      , Set.notMember door found -- but not keys we already have
      ]
  where
    stepFK FindKey{..} =
      [ FindKey (fkSteps+1) here

         -- Don't step over a key you haven't picked up yet
         | let k = world ! fkLocation
         , k < 'a' || 'z' < k || Set.member (toUpper k) found

         -- Take a step but don't stop on a wall or a closed door
         , here <- cardinal fkLocation
         , let cell = world ! here
         , cell /= '#'
         , cell < 'A' || 'Z' < cell || Set.member cell found
         ]
