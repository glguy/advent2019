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

import           Advent
import           Advent.Coord
import           Advent.Intcode
import           Advent.Search
import           Data.List

data SearchState = SearchState
  { onOxygen :: !Bool  -- ^ Is the robot currently on the oxygen
  , distance :: !Int   -- ^ Commands issued so far
  , location :: !Coord -- ^ robot's current location
  , effect   :: Effect -- ^ robot control program state
  }

main :: IO ()
main =
  do [inp] <- getParsedLines 15 memoryParser

     let outs = bfsOn location step1 (SearchState False 0 origin (run (new inp)))
         Just part1 = find onOxygen outs
     print (distance part1)

     -- restart the BFS from the oxygen location with distance counter reset
     print $ distance $ last $ bfsOn location step1 part1{distance = 0}

-- | Advance a robot one step, update its location
step1 :: SearchState -> [SearchState]
step1 SearchState{..} =
  case effect of
    Input f ->
      do (i,g)       <- [(1,above),(2,below),(3,left),(4,right)]
         (oxygen, e) <- case f i of
                          Output 1 e -> [(False, e)]
                          Output 2 e -> [(True , e)]
                          _          -> []
         [SearchState oxygen (distance + 1) (g location) e]
    _ -> error "Expected input"