{-# Language TypeApplications, RankNTypes, OverloadedStrings, ScopedTypeVariables, DataKinds #-}
{-|
Module      : Main
Description : Day 22 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/22>

>>> driver2 [DealNew] 1 10 <$> [0..9]
[9,8,7,6,5,4,3,2,1,0]

>>> driver2 [Cut 3] 1 10 <$> [0..9]
[3,4,5,6,7,8,9,0,1,2]

>>> driver2 [Cut (-4)] 1 10 <$> [0..9]
[6,7,8,9,0,1,2,3,4,5]

>>> driver2 [DealInc 3] 1 10 <$> [0..9]
[0,7,4,1,8,5,2,9,6,3]

>>> driver2 [DealInc 7, DealNew, DealNew] 1 10 <$> [0..9]
[0,3,6,9,2,5,8,1,4,7]

>>> driver2 [Cut 6, DealInc 7, DealNew] 1 10 <$> [0..9]
[3,0,7,4,1,8,5,2,9,6]

>>> driver2 [DealInc 7, DealInc 9, Cut (-2)] 1 10 <$> [0..9]
[6,3,0,7,4,1,8,5,2,9]

>>> driver2 [DealNew, Cut (-2), DealInc 7, Cut 8, Cut (-4), DealInc 7, Cut 3, DealInc 9, DealInc 3, Cut (-1)] 1 10 <$> [0..9]
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

data Command
  = Cut     Integer
  | DealInc Integer
  | DealNew
  deriving Show

parseCommand :: Parser Command
parseCommand
    = Cut     <$ "cut " <*> number
  <|> DealInc <$ "deal with increment " <*> number
  <|> DealNew <$ "deal into new stack"

------------------------------------------------------------------------
-- Composite shuffles
------------------------------------------------------------------------

toComposite :: KnownNat n => Command -> Composite n
toComposite DealNew     = Composite (-1) 1
toComposite (Cut     i) = Composite 1 (fromInteger i)
toComposite (DealInc i) = Composite (fromInteger i) 0

data Composite n = Composite
  { compDealInc :: !(Mod n)
  , compCut     :: !(Mod n)
  }
  deriving Show

apply :: KnownNat n => Composite n -> Mod n -> Mod n
apply c i = i * compDealInc c - compCut c

invert :: KnownNat n => Composite n -> Composite n
invert (Composite a b) = Composite (recip a) (-b/a)

instance KnownNat n => Semigroup (Composite n) where
  Composite a b <> Composite c d = Composite (a*c) (c*b+d)

instance KnownNat n => Monoid (Composite n) where
  mempty = Composite 1 0

------------------------------------------------------------------------
-- Driver code
------------------------------------------------------------------------

-- y = ax-b
-- y + b = ax
-- y/a + b/a = x

main :: IO ()
main =
  do inp <- getParsedLines 22 parseCommand
     print (driver1 inp                           10007 2019)
     print (driver2 inp 101741582076661 119315717514047 2020)

driver1 :: [Command] -> Natural -> Natural -> Natural
driver1 commands =
  runModFn (apply (foldMap toComposite commands))

driver2 :: [Command] -> Natural -> Natural -> Natural -> Natural
driver2 commands iterations =
  runModFn (apply (invert (stimes iterations (foldMap toComposite commands))))

runModFn :: (forall n. KnownNat n => Mod n -> Mod n) -> Natural -> Natural -> Natural
runModFn f modulus =
  case someNatVal modulus of
    SomeNat (_ :: p m) -> getNatVal . f @m . fromIntegral
