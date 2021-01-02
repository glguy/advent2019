{-# Language RankNTypes, OverloadedStrings, ViewPatterns #-}
{-|
Module      : Main
Description : Day 22 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/22>

>>> let shuffleTest cmds = invert (techsToLinearFn cmds) `withModulus` 10 <$> [0..9]

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

import Advent              (Parser, getParsedLines, number)
import Control.Applicative ((<|>))
import Data.Semigroup      (stimes)
import GHC.Natural         (Natural)
import GHC.TypeNats        (KnownNat, SomeNat(..), someNatVal)
import Data.Mod            (Mod, unMod)

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

-- | Compute function for a shuffle instruction mapping cards
-- in the shuffled deck to positions in the shuffled deck.
techToLinearFn :: KnownNat n => Technique -> LinearFn (Mod n)
techToLinearFn DealNew     = LinearFn (-1) (-1)          -- λx. -x-1
techToLinearFn (Cut     i) = LinearFn 1 (-fromInteger i) -- λx. x-i
techToLinearFn (DealInc i) = LinearFn (fromInteger i) 0  -- λx. ix

-- | Construts the linear function corresponding to applying the
-- given shuffles in order from left to right.
techsToLinearFn :: KnownNat n => [Technique] -> LinearFn (Mod n)
techsToLinearFn = foldMap techToLinearFn

------------------------------------------------------------------------
-- Linear functions
------------------------------------------------------------------------

-- | Linear functions: @Linear a b ~ λx. ax+b@
data LinearFn a = LinearFn !a !a
  deriving Show

apply :: Num a => LinearFn a -> a -> a
apply (LinearFn a b) x = a * x + b

invert :: Fractional a => LinearFn a -> LinearFn a
invert (LinearFn a b) = LinearFn (1/a) (-b/a)

-- | Reverse-composition of linear functions
--
-- >>> let f = LinearFn 1 2
-- >>> let g = LinearFn 3 4
-- >>> (f <> g) `apply` 10
-- 40
-- >>> g `apply` (f `apply` 10)
-- 40
instance Num a => Semigroup (LinearFn a) where
  LinearFn c d <> LinearFn a b = LinearFn (a*c) (b + a*d)

instance Num a => Monoid (LinearFn a) where
  mempty = LinearFn 1 0

------------------------------------------------------------------------
-- Driver code
------------------------------------------------------------------------

main :: IO ()
main =
  do techniques <- getParsedLines 22 parseTechnique

     let shuffle :: KnownNat n => LinearFn (Mod n)
         shuffle = techsToLinearFn techniques

     print ((shuffle `withModulus` 10007) 2019)

     let iterations  = 101741582076661 :: Int
         decksize    = 119315717514047
     print ((stimes iterations (invert shuffle) `withModulus` decksize) 2020)

withModulus ::
  (forall n. KnownNat n => LinearFn (Mod n)) ->
  Natural -> Natural -> Natural
f `withModulus` (someNatVal -> SomeNat m) = unMod . asMod m . apply f . fromIntegral

asMod :: proxy n -> Mod n -> Mod n
asMod _ x = x
