{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 12 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/12>

-}
module Main (main) where

import           Advent
import           Data.List
import qualified Data.Set as Set

parseMoon :: Parser Coord
parseMoon = C <$ "<x=" <*> number <* ", y=" <*> number <* ", z=" <*> number <* ">"

data Coord = C { getx, gety, getz :: !Int }
  deriving Show

data Particle = P !Int !Int
  deriving (Eq, Ord, Show)

newParticle :: Int -> Particle
newParticle x = P x 0

main :: IO ()
main =
  do inp <- getParsedLines 12 parseMoon

     let xs = map (newParticle . getx) inp
         ys = map (newParticle . gety) inp
         zs = map (newParticle . getz) inp

         x1 = iterate stepParticles xs !! 1000
         y1 = iterate stepParticles ys !! 1000
         z1 = iterate stepParticles zs !! 1000

     print $ sum $ map energy $ transpose [x1, y1, z1]

     let xn = repeatLength (iterate stepParticles xs)
         yn = repeatLength (iterate stepParticles ys)
         zn = repeatLength (iterate stepParticles zs)

     print (foldl1 lcm [xn, yn, zn])

energy :: [Particle] -> Int
energy ps = sum [ abs x | P x _ <- ps ] * sum [ abs v | P _ v <- ps ]

repeatLength :: Ord a => [a] -> Int
repeatLength = go Set.empty
  where
    go seen (x:xs)
      | Set.member x seen = Set.size seen
      | otherwise         = go (Set.insert x seen) xs
    go _ [] = error "No loop"

move :: Particle -> Particle
move (P x dx) = P (x+dx) dx

stepParticles :: [Particle] -> [Particle]
stepParticles ps =
   [ move (foldl' gravity p ps) | p <- ps ]

gravity :: Particle -> Particle -> Particle
gravity (P x v) (P y _) = P x (v + g)
  where
    g = case compare x y of
          LT -> 1
          GT -> -1
          EQ -> 0
