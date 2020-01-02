{-# Language RecordWildCards #-}
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
     let part1:_ = filter onOxygen (explore (newSearchState intcode))
     print (distance part1)
     print (distance (last (explore part1{distance = 0})))

data SearchState = SearchState
  { onOxygen :: !Bool  -- ^ Is the robot currently on the oxygen
  , distance :: !Int   -- ^ Commands issued so far
  , location :: !Coord -- ^ robot's current location
  , effect   :: Effect -- ^ robot control program state
  }

-- | Initial search state starting from assumed non-oxygen at the origin.
newSearchState :: [Int] {- ^ intcode -} -> SearchState
newSearchState = SearchState False 0 origin . run . new

-- | Breadth-first exploration of the maze
explore :: SearchState -> [SearchState]
explore = bfsOn location singleStep

-- | Generate the list of single steps that can be taken from a particular position.
singleStep :: SearchState -> [SearchState]
singleStep SearchState{..} =
  [ SearchState (o == 2) (distance + 1) (move location) e
    | Input f     <- [effect]
    , (i,move)    <- [(1,above),(2,below),(3,left),(4,right)]
    , Output o e  <- [f i]
    , o > 0
    ]
