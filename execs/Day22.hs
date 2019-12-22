{-# Language TypeApplications, RankNTypes, OverloadedStrings, ScopedTypeVariables, DataKinds #-}
{-|
Module      : Main
Description : Day 22 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/22>

>>> let shuffleTest cmds = runModFn (apply (commandsToLinear cmds)) 10 <$> [0..9]

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
toLinear :: KnownNat n => Technique -> Linear (Mod n)
toLinear DealNew     = Linear (-1) (-1)
toLinear (Cut     i) = Linear 1 (fromInteger i)
toLinear (DealInc i) = Linear (1 / fromInteger i) 0

-- | Construts the linear function corresponding to applying the
-- given shuffles in order from left to right.
commandsToLinear :: KnownNat n => [Technique] -> Linear (Mod n)
commandsToLinear = foldMap toLinear

------------------------------------------------------------------------
-- Linear functions
------------------------------------------------------------------------

-- | Linear functions: @Linear a b ~ λx. ax+b@
data Linear a = Linear !a !a
  deriving Show

apply :: Num a => Linear a -> a -> a
apply (Linear a b) x = a * x + b

invert :: Fractional a => Linear a -> Linear a
invert (Linear a b) = Linear (1/a) (-b/a)

-- | Composition of linear functions
--
-- >>> let f = Linear 1 2
-- >>> let g = Linear 3 4
-- >>> (f <> g) `apply` 10
-- 36
-- >>> f `apply` (g `apply` 10)
-- 36
instance Num a => Semigroup (Linear a) where
  Linear a b <> Linear c d = Linear (a*c) (b + a*d)

-- | @mempty ~ λx. x@
instance Num a => Monoid (Linear a) where
  mempty = Linear 1 0

------------------------------------------------------------------------
-- Driver code
------------------------------------------------------------------------

main :: IO ()
main =
  do inp <- getParsedLines 22 parseTechnique
     print (driver1 inp                           10007 2019)
     print (driver2 inp 101741582076661 119315717514047 2020)

driver1 :: [Technique] -> Natural -> Natural -> Natural
driver1 commands =
  runModFn (apply (invert (commandsToLinear commands)))

driver2 :: [Technique] -> Natural -> Natural -> Natural -> Natural
driver2 commands iterations =
  runModFn (apply (stimes iterations (commandsToLinear commands)))

runModFn :: (forall n. KnownNat n => Mod n -> Mod n) -> Natural -> Natural -> Natural
runModFn f modulus =
  case someNatVal modulus of
    SomeNat (_ :: p m) -> getNatVal @m . f . fromIntegral
