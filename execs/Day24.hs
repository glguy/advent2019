{-# Language OverloadedStrings #-}
{-# Options_GHC -w #-}
{-|
Module      : Main
Description : Day 24 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/24>

-}
module Main (main) where

import           Advent
import           Advent.Coord
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

main :: IO ()
main =
  do inp <- bugCoords <$> getInputLines 24
     print (biodiversity (findDup (iterate (update (filter inside . cardinal)) inp)))

     let inp3 = Set.map to3 inp
     print (Set.size (iterate (update cardinal3) inp3 !! 200))

update :: Ord a => (a -> [a]) -> Set a -> Set a
update adjacents m
  = Set.filter rule
  $ Set.union m
  $ Set.fromList
  $ adjacents =<< Set.toList m
  where
    rule k = 1 == n || 2 == n && Set.notMember k m
      where n = count (`Set.member` m) (adjacents k)

bugCoords :: [String] -> Set Coord
bugCoords xs = Set.fromList [ addCoord (C (-2) (-2)) k | (k, '#') <- coordLines xs]

biodiversity :: Set Coord -> Int
biodiversity m = sum [ 2^((2+y)*5+(2+x)) | c@(C y x) <- Set.toList m]

findDup :: Ord a => [a] -> a
findDup = go Set.empty
  where
    go _    [] = error "no duplicates"
    go seen (x:xs)
       | Set.member x seen = x
       | otherwise         = go (Set.insert x seen) xs

inside :: Coord -> Bool
inside (C y x) = -2 <= x && -2 <= y && x <= 2 && y <= 2

------------------------------------------------------------------------
-- 3-dimensional recursive board coordinates
------------------------------------------------------------------------

data C3 = C3 !Int !Int !Int
  deriving (Eq, Ord, Show)

to3 :: Coord -> C3
to3 (C y x) = C3 0 y x

cardinal3, left3, right3, above3, below3 :: C3 -> [C3]

cardinal3 c =
  concat [above3 c, left3 c, right3 c, below3 c]

left3 (C3 d y x)
  | x == -2        = [C3 (d-1) 0 (-1)]
  | x == 1, y == 0 = [C3 (d+1) i 0 | i <- [-2..2]]
  | otherwise      = [C3 d y (x-1)]

right3 (C3 d y x)
  | x == 2          = [C3 (d-1) 0 1]
  | x == -1, y == 0 = [C3 (d+1) i (-2) | i <- [-2..2]]
  | otherwise       = [C3 d y (x+1)]

below3 (C3 d y x)
  | y == 2          = [C3 (d-1) 1 0]
  | y == -1, x == 0 = [C3 (d+1) (-2) i | i <- [-2..2]]
  | otherwise       = [C3 d (y+1) x]

above3 (C3 d y x)
  | y == -2        = [C3 (d-1) (-1) 0]
  | y == 1, x == 0 = [C3 (d+1) 2 i | i <- [-2..2]]
  | otherwise      = [C3 d (y-1) x]
