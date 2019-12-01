{-# Language TypeFamilies #-}
module Advent.ListParser where

import Text.Megaparsec
import Data.Foldable (toList)

newtype ListParser a = ListParser [a]
  deriving (Read, Show)

instance (Show a, Ord a) => Stream (ListParser a) where
  type Token (ListParser a) = a
  type Tokens (ListParser a) = [a]

  tokensToChunk _ xs = xs
  chunkToTokens _ xs = xs
  chunkLength _ = length
  take1_ (ListParser []) = Nothing
  take1_ (ListParser (x:xs)) = Just (x, ListParser xs)
  takeN_ n (ListParser s)
    | n <= 0 = Just ([], ListParser s)
    | null s = Nothing
    | otherwise = case splitAt n s of
                    (a, b) -> Just (a, ListParser b)
  takeWhile_ p (ListParser xs) =
    case span p xs of
      (a, b) -> (a, ListParser b)
  showTokens _ tokens = show (toList tokens)
  reachOffset offset state =
    (pstateSourcePos state, "<list stream line>", state)
