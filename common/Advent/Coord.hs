{-|
Module      : Advent.Coord
Description : Row-major coordinates
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

-}
{-# Language BangPatterns, TypeFamilies, TypeOperators, DeriveGeneric #-}
module Advent.Coord where

import           Data.Foldable
import           Data.Ix
import           Data.Map (Map)
import qualified Data.Map as Map
import           GHC.Arr
import           GHC.Generics

data Coord = C !Int !Int
  deriving (Read, Show, Ord, Eq, Generic)

coordRow, coordCol :: Coord -> Int
coordRow (C row _) = row
coordCol (C _ col) = col

instance Ix Coord where
  unsafeIndex (C lorow locol, C _hirow hicol) (C row col) =
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

turnLeft, turnRight, turnAround :: Coord -> Coord
turnLeft  (C y x) = C (-x) y
turnRight (C y x) = C x (-y)
turnAround (C y x) = C (-y) (-x)

-- | Compute the Manhattan distance between two coordinates
manhattan :: Coord -> Coord -> Int
manhattan (C x y) (C u v) = abs (x-u) + abs (y-v)

-- | Compute the 4 cardinal neighbors of a coordinate: north, south, east, west
cardinal :: Coord -> [Coord]
cardinal c = c `seq` [above c, left c, right c, below c]

-- | Compute the 8 cardinal neighbors and diagonal neighbors
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

north :: Coord
north = C (-1) 0

addCoord :: Coord -> Coord -> Coord
addCoord (C y x) (C v u) = C (y+v) (x+u)

drawCoords :: Map Coord Char -> String
drawCoords pixels = unlines [[pixel (C y x) | x <- [minx .. maxx]] | y <- [miny .. maxy]]
  where
    pixel c = Map.findWithDefault ' ' c pixels
    Just (C miny minx, C maxy maxx) = boundingBox (Map.keys pixels)

coordLines :: [String] -> [(Coord, Char)]
coordLines rows = [(C y x, z) | (y,row) <- zip [0..] rows, (x,z) <- zip [0..] row]
