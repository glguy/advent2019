{-|
Module      : Advent.Queue
Description : Banker's queue implementation
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

-}
{-# Language PatternSynonyms, ViewPatterns #-}
{-# Options_GHC -Wno-name-shadowing #-}
module Advent.Queue (Queue(Empty, (:<|)), (|>), singleton, fromList, snoc, pop, appendList) where

import Data.Foldable (Foldable(..))
import Data.Monoid   (Dual(..))
import Data.Coerce   (coerce)

-- | FIFO Queue implementation
data Queue a = Queue [a] !Int [a] !Int

{-# COMPLETE (:<|), Empty #-}

-- | Empty queue
--
-- >>> Empty :: Queue Char
-- fromList ""
pattern Empty :: Queue a
pattern Empty <- Queue [] _ _ _
  where
    Empty = Queue [] 0 [] 0

-- | Pattern for 'pop'
--
-- >>> let x :<| xs = fromList "abc" in (x, xs)
-- ('a',fromList "bc")
pattern (:<|) :: a -> Queue a -> Queue a
pattern x :<| xs <- (pop -> Just (x, xs))

-- | Add an element to the end of a queue. See: 'snoc'
--
-- >>> fromList "abc" |> 'z'
-- fromList "abcz"
(|>) :: Queue a -> a -> Queue a
q |> x = snoc x q

-- | Fold over elements in the order they would be returned by pop
--
-- >>> toList (fromList "abc")
-- "abc"
instance Foldable Queue where
  toList    (Queue l _ r _) = l ++ reverse r
  foldr f z (Queue l _ r _) = foldr f (foldl (flip f) z r) l
  foldMap f (Queue l _ r _) = foldMap f l <> getDual (foldMap (coerce f) r)

-- | Renders using 'fromList' syntax.
--
-- >>> show (fromList "example")
-- "fromList \"example\""
instance Show a => Show (Queue a) where
  showsPrec p q
    = showParen (p >= 11)
    $ showString "fromList "
    . shows (toList q)

-- >>> read "fromList \"example\"" :: Queue Char
-- fromList "example"
instance Read a => Read (Queue a) where
  readsPrec prec
    = readParen (prec >= 11) $ \str ->
      do ("fromList", str) <- lex str
         (xs,         str) <- reads str
         return (fromList xs, str)

-- | Construct a queue from a single element.
--
-- >>> singleton 'a'
-- fromList "a"
singleton :: a -> Queue a
singleton x = Queue [x] 1 [] 0

-- | Construct a queue from a list. The head of the list will
-- be the first element returned by 'pop'
fromList :: [a] -> Queue a
fromList xs = Queue xs (length xs) [] 0

-- | Internal smart constructor for building queues that maintain
-- the invariant that the rear component is never longer than the
-- front component.
mkQueue :: Queue a -> Queue a
mkQueue q@(Queue f lenF r lenR)
  | lenR <= lenF = q
  | otherwise    = Queue (f ++ reverse r) (lenF + lenR) [] 0

-- | Add a new element to the end of a queue.
--
-- >>> snoc 'z' (fromList "abc")
-- fromList "abcz"
snoc :: a -> Queue a -> Queue a
snoc x (Queue f lenF r lenR) = mkQueue (Queue f lenF (x:r) (1+lenR))

-- | Append many items onto a queue. The items will pop from the queue
-- in the same order as they are in the given list.
--
-- >>> appendList "abc" (fromList "xyz")
-- fromList "xyzabc"
appendList :: [a] -> Queue a -> Queue a
appendList xs q = foldl' (|>) q xs

-- | Remove an element from the front of a queue and a new queue
-- without that element.
--
-- >>> pop (fromList "abc")
-- Just ('a',fromList "bc")
pop :: Queue a -> Maybe (a, Queue a)
pop (Queue (x:f) lenF r lenR) = Just (x, mkQueue (Queue f (lenF-1) r lenR))
pop _                         = Nothing
