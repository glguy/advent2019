{-# Language TypeApplications, RankNTypes, OverloadedStrings, ScopedTypeVariables, DataKinds #-}
{-|
Module      : Main
Description : Day 22 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/22>

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
apply c i = (compCut c + i) / compDealInc c

unapply :: KnownNat n => Composite n -> Mod n -> Mod n
unapply c i = i * compDealInc c - compCut c

instance KnownNat n => Semigroup (Composite n) where
  Composite a b <> Composite c d = Composite (a*c) (c*b+d)

instance KnownNat n => Monoid (Composite n) where
  mempty = Composite 1 0

------------------------------------------------------------------------
-- Driver code
------------------------------------------------------------------------

main :: IO ()
main =
  do inp <- getParsedLines 22 parseCommand
     print (driver1 inp                           10007 2019)
     print (driver2 inp 101741582076661 119315717514047 2020)

driver1 :: [Command] -> Natural -> Natural -> Natural
driver1 commands =
  runModFn (unapply (foldMap toComposite commands))

driver2 :: [Command] -> Natural -> Natural -> Natural -> Natural
driver2 commands iterations =
  runModFn (apply (stimes iterations (foldMap toComposite commands)))

runModFn :: (forall n. KnownNat n => Mod n -> Mod n) -> Natural -> Natural -> Natural
runModFn f n =
  case someNatVal n of
    SomeNat (_ :: p m) -> getNatVal . f @m . fromIntegral
