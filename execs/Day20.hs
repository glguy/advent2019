{-# Language BlockArguments #-}
{-|
Module      : Main
Description : Day 20 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/20>

-}
module Main (main) where

import           Advent
import           Advent.Coord
import           Advent.Search
import           Data.Char
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Array.Unboxed

main :: IO ()
main =
  do inp <- coordLines <$> getInputLines 20
     let Just b = boundingBox (map fst inp)
         world  = listArray b (map snd inp)
         labels = findLabels world
         Just [end  ] = Map.lookup "ZZ" labels
         Just [start] = Map.lookup "AA" labels

         links = findLinks labels
         jumps = shortcuts world (concat labels)

         outside = mkIsOutside labels

         layerChange p
           | outside p = -1
           | otherwise = 1

     print (search (const 0)   jumps links start end)
     print (search layerChange jumps links start end)

-- | Build predicate for coordinates on outer wall.
mkIsOutside ::
  Map String [Coord] {- ^ labeled coordinates -} ->
  Coord -> Bool
mkIsOutside labels = \(C y x) -> x == xhi || x == xlo || y == yhi || y == ylo
  where
    Just (C ylo xlo, C yhi xhi) = boundingBox (concat labels)

toArray :: [(Coord, Char)] -> UArray Coord Char
toArray xs = listArray b (map snd xs)
  where
    Just b = boundingBox (map fst xs)

data Pos = Pos !Coord !Int
 deriving (Eq, Ord, Show)

search ::
  (Coord -> Int)           {- ^ layer change   -} ->
  Map Coord [(Coord, Int)] {- ^ maze movements -} ->
  Map Coord Coord          {- ^ warp links     -} ->
  Coord                    {- ^ start position -} ->
  Coord                    {- ^ end position   -} ->
  Int                      {- ^ steps to end   -}
search delta jumps links start end = snd $ head $ filter isDone $ astar step (Pos start 0)
  where
   isDone (p,_) = Pos end 0 == p

   step (Pos here depth) =
     -- travel through a warp tile
     [ (Pos exit depth', cost + 1, 0)  |
          (enter, cost) <- Map.findWithDefault [] here jumps
        , exit          <- maybe [] pure (Map.lookup enter links)
        , let depth' = depth + delta enter
        , depth' >= 0
        ] ++
     -- finish maze
     [ (Pos enter 0, cost, 0)
          | depth == 0
          , (enter, cost) <- Map.findWithDefault [] here jumps
          , enter == end
        ]

-- | Find output destinations for each warp tile.
findLinks ::
  Map String [Coord] {- ^ labeled tiles -} ->
  Map Coord Coord    {- ^ warp links    -}
findLinks xs =
  Map.fromList
    do [p1,p2] <- Map.elems xs
       [(p1,p2), (p2,p1)]

-- | Find labeled coordinates.
findLabels :: UArray Coord Char -> Map String [Coord]
findLabels m =
  Map.fromListWith (++)
    [ (x,[k])
    | (k,'.') <- assocs m

    , x <- check k (left . left) left  ++
           check k right (right . right) ++
           check k (above . above) above ++
           check k below (below . below)
    ]
  where
    check k f g = [ [a,b] | let a = at (f k)
                          , let b = at (g k)
                          , isAlpha a, isAlpha b]
    at i = m ! i


-- | Given a list of starting positions find map of destinations
-- and costs to those destinations.
shortcuts :: UArray Coord Char -> [Coord] -> Map Coord [(Coord,Int)]
shortcuts world targets = Map.fromList [(start, travelFrom start) | start <- targets]
  where
    targetSet = Set.fromList targets

    travelFrom src =
      [ (dst,n)
         | (dst,n) <- astar step src
         , Set.member dst targetSet
         , dst /= src
         ]

    step here = [(there, 1, 0) | there <- cardinal here, world ! there == '.']
