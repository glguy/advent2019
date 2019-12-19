{-|
Module      : Main
Description : Generalized search functions
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Advent.Search where

import qualified Advent.PQueue as PQueue
import qualified Advent.Queue  as Queue
import           Data.Foldable
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet

{-# INLINE dfs #-}
dfs :: Ord a => (a -> [a]) -> a -> [a]
dfs = dfsOn id

dfsOn :: Ord r => (a -> r) -> (a -> [a]) -> a -> [a]
dfsOn rep next start = loop Set.empty [start]
  where
    loop _ [] = []
    loop seen (x:xs)
      | Set.member r seen =     loop seen xs
      | otherwise         = x : loop seen1 (next x ++ xs)
      where
        r     = rep x
        seen1 = Set.insert r seen

{-# INLINE bfs #-}
bfs :: Ord a => (a -> [a]) -> a -> [a]
bfs = bfsOn id

-- | Enumerate the reachable states in breadth-first order
-- given a successor state function and initial state.
--
-- States are compared for equality using the representative
-- function. If the representatives are equal the state is
-- considered already visited.
{-# INLINE [0] bfsOn #-}
bfsOn ::
  Ord r =>
  (a -> r)   {- ^ representative function   -} ->
  (a -> [a]) {- ^ successor state generator -} ->
  a          {- ^ initial state             -} ->
  [a]        {- ^ reachable states          -}
bfsOn rep next start = bfsOnN rep next [start]

{-# INLINE [0] bfsOnN #-}
bfsOnN ::
  Ord r =>
  (a -> r)   {- ^ representative function   -} ->
  (a -> [a]) {- ^ successor state generator -} ->
  [a]         {- ^ initial state             -} ->
  [a]        {- ^ reachable states          -}
bfsOnN rep next start = loop Set.empty (Queue.fromList start)
  where
    loop _ Queue.Empty = []
    loop seen (x Queue.:<| q1)
      | Set.member r seen =     loop seen  q1
      | otherwise         = x : loop seen1 q2
      where
        r     = rep x
        seen1 = Set.insert r seen
        q2    = Queue.appendList (next x) q1

{-# RULES "bfsOn/Int" bfsOn = bfsOnInt #-}
{-# INLINE bfsOnInt #-}
bfsOnInt :: (a -> Int) -> (a -> [a]) -> a -> [a]
bfsOnInt rep next start = loop IntSet.empty (Queue.singleton start)
  where
    loop seen q =
      case q of
        Queue.Empty -> []
        x Queue.:<| q1
          | IntSet.member r seen ->     loop seen  q1
          | otherwise            -> x : loop seen1 q2
          where
            r     = rep x
            seen1 = IntSet.insert r seen
            q2    = Queue.appendList (next x) q1

{-# INLINE astar #-}
astar :: Ord a => (a -> [(a,Int,Int)]) -> a -> [(a,Int)]
astar = astarOn id

{-# INLINE astarOn #-}
astarOn ::
  Ord b =>
  (a -> b)             {- ^ state characterization                                   -} ->
  (a -> [(a,Int,Int)]) {- ^ step function (new state, step cost, distance heuristic) -} ->
  a                    {- ^ starting state                                           -} ->
  [(a,Int)]            {- ^ list of states visited                                   -}
astarOn rep nexts start = go Set.empty (PQueue.singleton 0 (0,start))
  where
    go seen work =
      case work of
        PQueue.Empty -> []
        (cost,x) PQueue.:<| work1
          | Set.member r seen -> go seen work1
          | otherwise         -> (x,cost) : go seen' work2
          where
            r = rep x
            seen' = Set.insert r seen
            work2 = foldl' addWork work1 (nexts x)
            addWork w (x',stepcost,heuristic) =
              let cost' = cost + stepcost
              in cost' `seq`
                 PQueue.insert (cost' + heuristic) (cost', x') w
