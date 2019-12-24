{-# Language OverloadedStrings #-}
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

main :: IO ()
main =
  do inp <- bugCoords <$> getInputLines 24

     print (biodiversity (findDup (iterate update inp)))

     let inp3 = Set.map to3 inp
     print (Set.size (iterate update inp3 !! 200))

update :: (Ord a, Neighbors a) => Set a -> Set a
update m = Set.filter rule
         $ Set.union m
         $ Set.fromList
         $ adjacents =<< Set.toList m
  where
    rule k = 1 == n || 2 == n && Set.notMember k m
      where n = count (`Set.member` m) (adjacents k)

bugCoords :: [String] -> Set Coord
bugCoords xs = Set.fromList [k | (k, '#') <- coordLines xs]

biodiversity :: Set Coord -> Int
biodiversity m = sum [ i | (i,k) <- zip (iterate (2*) 1) coords, Set.member k m]
  where
    coords = [C y x | y <- [0..4], x <- [0..4]]

findDup :: Ord a => [a] -> a
findDup = go Set.empty
  where
    go _    [] = error "no duplicates"
    go seen (x:xs)
       | Set.member x seen = x
       | otherwise         = go (Set.insert x seen) xs

class    Neighbors a     where adjacents :: a -> [a]
instance Neighbors Coord where adjacents = filter inside . cardinal
instance Neighbors C3    where adjacents = cardinal3

inside :: Coord -> Bool
inside (C y x) = 0 <= x && 0 <= y && x < 5 && y < 5

------------------------------------------------------------------------
-- 3-dimensional recursive board coordinates
------------------------------------------------------------------------

data C3 = C3 !Int !Int !Int
  deriving (Eq, Ord, Show)

to3 :: Coord -> C3
to3 (C y x) = C3 0 y x

cardinal3, left3, right3, above3, below3 :: C3 -> [C3]

cardinal3 c =
  concat [left3 c, right3 c, above3 c, below3 c]

left3 (C3 d y x)
  | x == 0         = [C3 (d-1) 2 1]
  | x == 3, y == 2 = [C3 (d+1) i 4 | i <- [0..4]]
  | otherwise      = [C3 d y (x-1)]

right3 (C3 d y x)
  | x == 4         = [C3 (d-1) 2 3]
  | x == 1, y == 2 = [C3 (d+1) i 0 | i <- [0..4]]
  | otherwise      = [C3 d y (x+1)]

below3 (C3 d y x)
  | y == 4         = [C3 (d-1) 3 2]
  | y == 1, x == 2 = [C3 (d+1) 0 i | i <- [0..4]]
  | otherwise      = [C3 d (y+1) x]

above3 (C3 d y x)
  | y == 0         = [C3 (d-1) 1 2]
  | y == 3, x == 2 = [C3 (d+1) 4 i | i <- [0..4]]
  | otherwise      = [C3 d (y-1) x]
