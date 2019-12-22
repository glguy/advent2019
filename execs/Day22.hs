{-# Language RankNTypes, OverloadedStrings, DeriveFunctor #-}
{-|
Module      : Main
Description : Day 22 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/22>

>>> let shuffleTest cmds = techsToLinearFn cmds `withModulus` 10 <$> [0..9]

>>> shuffleTest [DealNew]
[9,8,7,6,5,4,3,2,1,0]

>>> shuffleTest [Cut 3]
[3,4,5,6,7,8,9,0,1,2]

>>> shuffleTest [Cut (-4)]
[6,7,8,9,0,1,2,3,4,5]

>>> shuffleTest [DealInc 3]
[0,7,4,1,8,5,2,9,6,3]

>>> shuffleTest [DealInc 7, DealNew, DealNew]
[0,3,6,9,2,5,8,1,4,7]

>>> shuffleTest [Cut 6, DealInc 7, DealNew]
[3,0,7,4,1,8,5,2,9,6]

>>> shuffleTest [DealInc 7, DealInc 9, Cut (-2)]
[6,3,0,7,4,1,8,5,2,9]

>>> shuffleTest [DealNew, Cut (-2), DealInc 7, Cut 8, Cut (-4), DealInc 7, Cut 3, DealInc 9, DealInc 3, Cut (-1)]
[9,2,5,8,1,4,7,0,3,6]

-}
module Main (main) where

import Advent                         (Parser, getParsedLines, number)
import Control.Applicative            ((<|>))
import Data.Semigroup                 (stimes)
import GHC.Natural                    (Natural)
import GHC.TypeNats                   (KnownNat, SomeNat(..), someNatVal)
import Math.NumberTheory.Moduli.Class (Mod, getNatVal)

------------------------------------------------------------------------
-- Parsing
------------------------------------------------------------------------

data Technique
  = Cut     Integer -- ^ cut N cards
  | DealInc Integer -- ^ deal with increment N
  | DealNew         -- ^ deal into new stack
  deriving Show

parseTechnique :: Parser Technique
parseTechnique
    = Cut     <$ "cut "                 <*> number
  <|> DealInc <$ "deal with increment " <*> number
  <|> DealNew <$ "deal into new stack"

------------------------------------------------------------------------
-- Shuffles
------------------------------------------------------------------------

-- | Compute function for a shuffle instruction mapping indexes in
-- the shuffled deck to positions in the source deck.
techToLinearFn :: Fractional a => Technique -> LinearFn a
techToLinearFn DealNew     = add (-1) (scale (-1) x_)
techToLinearFn (Cut     i) = add (fromInteger i) x_
techToLinearFn (DealInc i) = scale (1/fromInteger i) x_

-- | Construts the linear function corresponding to applying the
-- given shuffles in order from left to right.
techsToLinearFn :: Fractional a => [Technique] -> LinearFn a
techsToLinearFn = foldMap techToLinearFn

------------------------------------------------------------------------
-- Linear functions
------------------------------------------------------------------------

-- | Linear functions: @Linear a b ~ λx. ax+b@
data LinearFn a = LinearFn !a !a
  deriving (Functor, Show)

apply :: Num a => LinearFn a -> a -> a
apply (LinearFn a b) x = a * x + b

invert :: Fractional a => LinearFn a -> LinearFn a
invert (LinearFn a b) = LinearFn (1/a) (-b/a)

-- | Identity function
x_ :: Num a => LinearFn a
x_ = LinearFn 1 0

scale :: Num a => a -> LinearFn a -> LinearFn a
scale x (LinearFn a b) = LinearFn (x * a) (x * b)

add :: Num a => a -> LinearFn a -> LinearFn a
add x (LinearFn a b) = LinearFn a (x + b)

-- | Composition of linear functions
--
-- >>> let f = LinearFn 1 2
-- >>> let g = LinearFn 3 4
-- >>> (f <> g) `apply` 10
-- 36
-- >>> f `apply` (g `apply` 10)
-- 36
instance Num a => Semigroup (LinearFn a) where
  LinearFn a b <> LinearFn c d = LinearFn (a*c) (b + a*d)

-- | @'mempty' = 'x_'@
instance Num a => Monoid (LinearFn a) where
  mempty = x_

------------------------------------------------------------------------
-- Driver code
------------------------------------------------------------------------

main :: IO ()
main =
  do techniques <- getParsedLines 22 parseTechnique

     let shuffle :: Fractional a => LinearFn a
         shuffle = techsToLinearFn techniques

     print ((invert shuffle `withModulus` 10007) 2019)

     let iterations = 101741582076661 :: Natural
     print ((stimes iterations shuffle `withModulus` 119315717514047) 2020)

withModulus ::
  (forall n. KnownNat n => LinearFn (Mod n)) ->
  Natural -> Natural -> Natural
f `withModulus` modulus =
  case someNatVal modulus of
    SomeNat p -> getNatVal . asMod p . apply f . fromIntegral

asMod :: proxy n -> Mod n -> Mod n
asMod _ x = x
