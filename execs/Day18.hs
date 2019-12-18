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
import           Control.Applicative
import           Data.List
import           Data.Char
import           Data.Set (Set)
import qualified Data.Set as Set
import           Control.Monad
import           Data.Array.Unboxed

main :: IO ()
main =
  do inp <- getParsedLines 18 (many anySingle)
     let inp1 = [ (C y x, col)
                 | (y,row) <- zip [0..] inp
                 , (x,col) <- zip [0..] row ]
         Just b = boundingBox (map fst inp1)
         world = listArray b (map snd inp1)
               :: UArray Coord Char

     let start = head [ k | (k,'@') <- inp1 ]
     let keyN = length (filter isLower (concat inp))

     print $ find (\s -> Set.size (akKeys (fst s)) == keyN) $ allKeys world (Set.singleton start)

     let world2 = world // [ (c,'#')  | c <- start : cardinal start ]
         start2 = Set.fromList
                  [ left (above start), right (above start),
                    left (below start), right (below start) ]

     print $ find (\s -> Set.size (akKeys (fst s)) == keyN) $ allKeys world2 start2

data AllKeys = AllKeys
  { akLocation :: !(Set Coord)
  , akKeys     :: !(Set Char)
  }
  deriving (Ord, Eq, Show)

allKeys :: UArray Coord Char -> Set Coord -> [(AllKeys, Int)]
allKeys world start =
  astar stepAK (AllKeys start Set.empty)
  where
    stepAK AllKeys{..} =
      do who   <- Set.toList akLocation
         (s,k) <- nextKeys world akKeys who
         [ (AllKeys
             (Set.insert (fkLocation s) (Set.delete who akLocation))
             (Set.insert k akKeys)
           , fkSteps s
           , 0) ]

data FindKey = FindKey
  { fkSteps    :: !Int
  , fkLocation :: !Coord
  }
  deriving Show

nextKeys :: UArray Coord Char -> Set Char -> Coord -> [ (FindKey, Char) ]
nextKeys world found start =
  [ (s,k')
      | s <- bfsOn fkLocation stepFK (FindKey 0 start)
      , let k = world ! fkLocation s
      , isLower k
      , let k' = toUpper k
      , Set.notMember k' found ]
  where
    stepFK FindKey{..} =
      do guard (let k = world ! fkLocation in
                k < 'a' || 'z' < k || Set.member (toUpper k) found)
         here <- cardinal fkLocation
         case world ! here of
           '#' -> []
           a | 'A' <= a, a <= 'Z', Set.notMember a found -> []
           _ -> [FindKey (fkSteps+1) here]
