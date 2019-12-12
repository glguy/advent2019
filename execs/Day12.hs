{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 12 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/12>

The stepping function is invertible, so any cycles must include
the starting point. This means to find a cycle we just search
for the starting point rather than remembering all states along
the way.

-}
module Main (main) where

import Advent (getParsedLines, Parser, manyTill, satisfy, number, sepBy)
import Data.List (transpose, elemIndex, foldl')
import Data.Char (isAlpha)

parseMoon :: Parser [Int]
parseMoon = "<" *> component `sepBy` ", " <* ">"
  where
    component = manyTill (satisfy isAlpha) "=" *> number

-- | One-dimensional particle with a position and velocity.
data Particle = P !Int !Int -- ^ position velocity
  deriving (Eq, Ord, Show)

-- | Build a stationary particle.
newParticle :: Int {- ^ position -} -> Particle
newParticle x = P x 0

main :: IO ()
main =
  do inp <- getParsedLines 12 parseMoon

     let xs = transpose (map (map newParticle) inp)

         x1 = [ iterate stepParticles x !! 1000 | x <- xs ]

     print (sum (map energy (transpose x1)))

     let xn = map (repeatLength . iterate stepParticles) xs

     print (foldl' lcm 1 xn)

-- | Compute the energy of a multi-dimensional particle given
-- its dimensional components.
energy :: [Particle] -> Int
energy ps = sum [ abs x | P x _ <- ps ] * sum [ abs v | P _ v <- ps ]

repeatLength :: Eq a => [a] -> Int
repeatLength [] = error "repeatList: no cycle"
repeatLength (x:xs) = 1 + n
  where Just n = elemIndex x xs

-- | Advance a particle by its current velocity.
move :: Particle -> Particle
move (P x dx) = P (x+dx) dx

-- | Single step of a one-dimensional, n-body system.
stepParticles :: [Particle] -> [Particle]
stepParticles ps = [ move (foldl' gravity p ps) | p <- ps ]

-- | Apply gravity to the first particle based on the second.
gravity :: Particle -> Particle -> Particle
gravity (P x v) (P y _) = P x (v + g)
  where
    g = case compare x y of
          LT -> 1
          GT -> -1
          EQ -> 0
