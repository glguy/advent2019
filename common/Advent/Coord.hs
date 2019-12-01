{-|
Module      : Advent.Coord
Description : Row-major coordinates
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

-}
{-# Language BangPatterns, TypeFamilies, TypeOperators, DeriveGeneric #-}
module Advent.Coord where

import Data.Foldable
import Data.Ix
import Data.MemoTrie
import GHC.Arr
import GHC.Generics

data Coord = C !Int !Int
  deriving (Read, Show, Ord, Eq, Generic)

coordRow, coordCol :: Coord -> Int
coordRow (C row _) = row
coordCol (C _ col) = col

instance Ix Coord where
  unsafeIndex (C lorow locol, C hirow hicol) (C row col) =
    (row - lorow) * (hicol - locol + 1) + (col - locol)

  inRange (C lorow locol, C hirow hicol) (C row col) =
    lorow <= row && row <= hirow &&
    locol <= col && col <= hicol

  range (C lorow locol, C hirow hicol) =
    [ C row col | row <- [lorow..hirow], col <- [locol..hicol]]

above, below, left, right :: Coord -> Coord
above (C y x) = C (y-1)  x
below (C y x) = C (y+1)  x
left  (C y x) = C  y    (x-1)
right (C y x) = C  y    (x+1)

-- | Compute the Manhattan distance between two coordinates
manhattan :: Coord -> Coord -> Int
manhattan (C x y) (C u v) = abs (x-u) + abs (y-v)

-- | Compute the cardinal neighbors of a coordinate: north, south, east, west
cardinal :: Coord -> [Coord]
cardinal c = c `seq` [above c, left c, right c, below c]

-- | Compute the cardinal neighbors of a coordinate: north, south, east, west
neighbors :: Coord -> [Coord]
neighbors c = c `seq` [above c, left c, right c, below c,
                       above (left c), above (right c),
                       below (left c), below (right c)]

boundingBox :: Foldable t => t Coord -> Maybe (Coord, Coord)
boundingBox t =
  case toList t of
    []         -> Nothing
    C y x : cs -> go y x y x cs
  where
    go loy lox hiy hix [] = Just (C loy lox, C hiy hix)
    go loy lox hiy hix (C y x : cs) = go (min loy y) (min lox x) (max hiy y) (max hix x) cs

origin :: Coord
origin = C 0 0

addCoord :: Coord -> Coord -> Coord
addCoord (C y x) (C v u) = C (y+v) (x+u)

instance HasTrie Coord where
  newtype Coord :->: r = CoordTrie { unCoordTrie :: Reg Coord :->: r }
  trie                 = trieGeneric CoordTrie
  untrie               = untrieGeneric unCoordTrie
  enumerate            = enumerateGeneric unCoordTrie
