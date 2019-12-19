{-# Language RankNTypes, MultiWayIf, GeneralizedNewtypeDeriving, BlockArguments, OverloadedStrings #-}
{-# Options_GHC -Wno-unused-top-binds #-}
{-|
Module      : Main
Description : Day 13 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/13>

-}
module Main (main) where

import           Advent
import           Advent.Intcode as I
import           Advent.Intcode.Conduino
import           Advent.Coord
import           Control.Exception
import           Data.Map (Map)
import qualified Data.Map as Map
import           Graphics.Vty
import           Data.Conduino
import           Data.Conduino.Combinators (sinkNull)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Cont
import           System.Exit

newtype M a = M { unM :: StateT GameState (ContT () IO) a }
  deriving (Functor, Applicative, Monad, MonadIO)

getGS :: M GameState
getGS = M get

modifyGS :: (GameState -> GameState) -> M ()
modifyGS f = M (modify' f)

newGameState :: GameState
newGameState = GameState
  { score = 0
  , video = Map.empty
  , history = [] }

main :: IO ()
main =
  do [inp] <- getParsedLines 13 memoryParser
     cfg   <- standardIOConfig
     bracket (mkVty cfg) shutdown \vty ->
       driver vty (set 0 2 (new inp))


data GameState = GameState
  { video :: !(Map Coord Char)
  , score :: !Int
  , history :: [Label (ContT () IO)]
  }

drawScreen :: Vty -> M ()
drawScreen vty =
  do gs <- getGS
     liftIO (update vty (picForImage (draw gs)))

joystick :: Vty -> Pipe i Int u M a
joystick vty =
  do snap <- lift (M (lift labelCC))
     lift (modifyGS \gs -> gs { history = snap : history gs })
     lift (drawScreen vty)
     loop
  where
    loop =
      do e <- liftIO (nextEvent vty)
         case e of
           EvKey KEsc   [] -> liftIO exitSuccess
           EvKey KLeft  [] -> yield (-1) >> joystick vty
           EvKey KRight [] -> yield 1    >> joystick vty
           EvKey KDown  [] -> yield 0    >> joystick vty
           EvKey (KChar 'u') [] ->
             do hs <- history <$> lift getGS
                case hs of
                  _:l:_ -> lift (M (lift (jump l)))
                  _     -> loop
           _            -> loop

writeToVideo :: Pipe Int o u M u
writeToVideo =
  awaitWith \x ->
  awaitWith \y ->
  awaitWith \t ->
  do let p = C (fromIntegral y) (fromIntegral x)
     lift $ modifyGS \gs ->
       if | -1 == x, 0 == y -> gs { score = t }
          | t == 0          -> gs { video = Map.delete p (video gs) }
          | otherwise       -> gs { video = (Map.insert p $! render t) (video gs) }
     writeToVideo

driver :: Vty -> Machine -> IO ()
driver vty mach
  = evalContT
  $ flip evalStateT newGameState
  $ unM
  $ runPipe
  $ gamePipe vty mach

gamePipe :: Vty -> Machine -> Pipe i o u M ()
gamePipe vty mach = joystick vty .| machinePipe mach .| writeToVideo .| sinkNull

render :: Int -> Char
render 1 = '█'
render 2 = '❑'
render 3 = '―'
render 4 = '●'
render _ = ' '

draw :: GameState -> Image
draw gs =
  case boundingBox (Map.keys (video gs)) of
    Nothing -> emptyImage
    Just (C miny minx, C maxy maxx) ->
      string defAttr ("Score: " ++ show (score gs)) <->
      vertCat [ horizCat [pixel (C y x) | x <- [minx .. maxx]] | y <- [miny .. maxy]]
      where
        pixel c = char defAttr (Map.findWithDefault ' ' c (video gs))

newtype Label m = Label (forall a. m a)

jump :: Label m -> m a
jump (Label m) = m

labelCC :: ContT r m (Label (ContT r m))
labelCC = ContT \k -> let rec = k (Label (ContT \_ -> rec)) in rec
