{-# Language RecordWildCards, ViewPatterns #-}
{-|
Module      : Main
Description : Day 15 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/15>

-}
module Main (main) where

import Advent        (getIntcodeInput)
import Advent.Coord  (Coord, above, below, left, right, origin)
import Advent.Search (bfsOn)
import Intcode       (Effect(..), run, new)

main :: IO ()
main =
  do intcode <- getIntcodeInput 15
     let part1:_ = filter onOxygen (search (newSearchState intcode))
     print (distance part1)
     print (distance (last (search part1{distance = 0})))

data SearchState = SearchState
  { onOxygen :: !Bool  -- ^ Is the robot currently on the oxygen
  , distance :: !Int   -- ^ Commands issued so far
  , location :: !Coord -- ^ robot's current location
  , effect   :: Effect -- ^ robot control program
  }

-- | Initial search state starting from assumed non-oxygen at the origin.
newSearchState :: [Int] {- ^ intcode -} -> SearchState
newSearchState = SearchState False 0 origin . run . new

-- | Breadth-first exploration of the maze
search :: SearchState -> [SearchState]
search = bfsOn location searchStep

-- | Generate the list of single steps that can be taken from a particular position.
searchStep :: SearchState -> [SearchState]
searchStep SearchState{..} =
  [ SearchState (o == 2) (distance + 1) (move location) e
  | (i,move) <- [(1,above),(2,below),(3,left),(4,right)]
  , (o,e)    <- effectStep i effect
  ]

-- | Give the robot a movement instruction and get the block status
effectStep :: Int {- ^ direction -} -> Effect -> [(Int, Effect)]
effectStep i (Input (($ i) -> Output o e)) | o > 0 = [(o,e)]
effectStep _ _                                     = []
