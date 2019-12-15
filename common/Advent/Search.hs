module Advent.Search where

import qualified Advent.PQueue as PQueue
import           Data.Foldable
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet

{-# INLINE dfs #-}
dfs :: Ord a => (a -> [a]) -> a -> [a]
dfs next start = aux start (const []) Set.empty
  where
    aux x rest seen
      | Set.member x seen = rest seen
      | otherwise = x : foldr aux rest (next x) (Set.insert x seen)

{-# INLINE bfs #-}
bfs :: Ord a => (a -> [a]) -> a -> [a]
bfs = bfsOn id

{-# INLINE [0] bfsOn #-}
bfsOn :: Ord b => (a -> b) -> (a -> [a]) -> a -> [a]
bfsOn rep next start = aux Set.empty [start] []
  where
    aux _    [] [] = []
    aux seen [] ys = aux seen (reverse ys) []
    aux seen (x:xs) ys
      | Set.member r seen = aux seen xs ys
      | otherwise = x : aux (Set.insert r seen) xs (next x ++ ys)
      where r = rep x

{-# RULES "bfsOn/Int" bfsOn = bfsOnInt #-}
{-# INLINE bfsOnInt #-}
bfsOnInt :: (a -> Int) -> (a -> [a]) -> a -> [a]
bfsOnInt rep next start = aux IntSet.empty [start] []
  where
    aux _    [] [] = []
    aux seen [] ys = aux seen (reverse ys) []
    aux seen (x:xs) ys
      | IntSet.member x' seen = aux seen xs ys
      | otherwise = x : aux (IntSet.insert x' seen) xs (next x ++ ys)
      where
        x' = rep x

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
