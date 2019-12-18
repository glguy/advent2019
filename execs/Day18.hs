{-# Language RecordWildCards, OverloadedStrings #-}
{-|
Module      : Main
Description : Day 18 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/18>

This solution used roughly 1GB of RAM and took
156 seconds to run both parts but was good enough
to finish the problem!

-}
module Main (main) where

import           Advent
import           Advent.Coord
import           Advent.Search
import           Control.Applicative
import           Data.List
import           Data.Char
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main =
  do inp <- getParsedLines 18 (many anySingle)
     let world = Map.fromList [ (C y x, col)
                                | (y,row) <- zip [0..] inp
                                , (x,col) <- zip [0..] row ]
     let start = head [ k | (k,'@') <- Map.toList world ]

     let keyN = Map.size (Map.filter isLower world)

     print $ fmap steps
           $ find (done keyN)
           $ bfsOn (\ss -> (keys ss, location ss)) (step1 world) (ss0 start [])

     let world2 = foldl' (\m k -> Map.insert k '#' m) world (start : cardinal start)
         starts = [ left (above start), right (above start),
                    left (below start), right (below start) ]

     print $ fmap steps
           $ find (done keyN)
           $ bfsOnN (\ss -> (keys ss, location ss)) (step1 world2)
                [ ss0 x (delete x starts) | x <- starts]

data SearchState = SS
  { steps :: !Int
  , keys  :: !(Set Char)
  , location :: !Coord
  , others   :: [Coord]
  }
  deriving Show

at = Map.findWithDefault '#'

done keyN ss = keyN == Set.size (keys ss)

ss0 = SS 0 Set.empty

step1 world SS{..} =
  do here <- cardinal location
     case at here world of
       '#' -> []
       '.' -> [SS (steps+1) keys here others]
       '@' -> [SS (steps+1) keys here others]
       a | isLower a ->
             do let everyone = here : others
                leader <- everyone
                [SS (steps+1) (Set.insert (toUpper a) keys) leader (delete leader everyone)]
         | isUpper a -> [SS (steps+1) keys here others | Set.member a keys ]
