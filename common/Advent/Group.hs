{-# Language TypeOperators, MultiParamTypeClasses #-}
module Advent.Group
  ( Group(inverse)
  , RightAction(rightAction)
  , type (><|)((:><|))
  ) where

import Data.Semigroup

class Monoid a => Group a where
  inverse :: a -> a

instance Num a => Group (Sum a) where
  inverse (Sum n) = Sum (negate n)

-- | Outer semi-direct product
data a ><| b = a :><| b deriving Show

instance (Semigroup a, Semigroup b, Group b, RightAction a b) => Semigroup (a ><| b)
  where
  (n1 :><| h1) <> (n2 :><| h2) = (rightAction n1 h1 <> rightAction n2 (inverse h1))
                            :><| (h1 <> h2)

instance (Semigroup a, Monoid a, Group b, RightAction a b) => Monoid (a ><| b) where
  mempty  = mempty :><| mempty
  mappend = (<>)

class Semigroup b => RightAction a b where
  rightAction :: a -> b -> a
