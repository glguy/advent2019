module Advent.Intcode.Conduino
  ( module Data.Conduino,
    module Data.Conduino.Combinators,

    -- * Intcode adapters
    machinePipe, effectPipe,
  ) where

import Advent.Intcode
import Data.Conduino
import Data.Conduino.Combinators
import Data.Void

machinePipe :: Machine -> Pipe Integer Integer Void m ()
machinePipe = effectPipe . run

effectPipe :: Effect -> Pipe Integer Integer Void m ()
effectPipe Halt         = return ()
effectPipe (Output o e) = yield o     >>  effectPipe e
effectPipe (Input  f  ) = awaitSurely >>= effectPipe . f
