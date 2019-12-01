module Advent.Search where

import Data.Foldable
import Data.Set ( Set )
import Data.IntSet ( IntSet )
import Data.IntMap ( IntMap )
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap

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
astarOn rep nexts start = go Set.empty (IntMap.singleton 0 [(0,start)])
  where

    go seen work =
      case pqueueNext work of
        Nothing -> []
        Just ((cost,x),work1)
          | Set.member r seen -> go seen work1
          | otherwise         -> (x,cost) : go seen' work2
          where
            r = rep x
            seen' = Set.insert r seen
            work2 = foldl' addWork work1 (nexts x)
            addWork w (x',stepcost,heuristic) =
              let cost' = cost + stepcost
              in cost' `seq`
                 pqueuePush (cost' + heuristic) (cost', x') w

pqueuePush :: Int -> a -> IntMap [a] -> IntMap [a]
pqueuePush k v = IntMap.alter aux k
  where
    aux Nothing = Just [v]
    aux (Just vs) = Just (v:vs)

pqueueNext :: IntMap [a] -> Maybe (a,IntMap [a])
pqueueNext q =
      do ((k,xs),q1) <- IntMap.minViewWithKey q
         case xs of
           [] -> error "Malformed queue"
           [x] -> Just (x,q1)
           x:xs -> let q2 = IntMap.insert k xs q1
                   in q2 `seq` Just (x,q2)
