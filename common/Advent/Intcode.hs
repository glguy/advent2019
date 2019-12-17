{-# Language DeriveTraversable, OverloadedStrings #-}
{-|
Module      : Advent.Intcode
Description : Intcode interpreter
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

This Intcode interpreter is defined across multiple Advent of Code days:

* <https://adventofcode.com/2019/day/2>
* <https://adventofcode.com/2019/day/5>
* <https://adventofcode.com/2019/day/7>
* <https://adventofcode.com/2019/day/9>

This implementation works with the following passes:

  1. Parse input text file into a list of numbers
  2. Execute op codes to extract the input/output "effects"
  3. Evaluate the effect as a function from a list of inputs to list of outputs

>>> intCodeToList [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] <$> [[0],[10]]
[[0],[1]]

>>> intCodeToList [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] <$> [[0],[10]]
[[0],[1]]

>>> :{
>>> intCodeToList
>>>   [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
>>>    1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
>>>    999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
>>> <$> [[7],[8],[9]]
>>> :}
[[999],[1000],[1001]]

-}
module Advent.Intcode
  (
  -- * Simple interface
  memoryParser,
  intCodeToList,

  -- * Machine state
  Machine(..), (!), new, set, memoryList,

  -- * Effects
  Effect(..), run, (>>>),

  -- * Small-step
  Step(..), step,

  -- * Opcodes
  Mode(..), Opcode(..), decode,
  ) where

import           Advent    (Parser, number, sepBy)
import           Data.Map (Map)
import           Data.Traversable (mapAccumL)
import qualified Data.Map as Map
import           Text.Show.Functions ()

------------------------------------------------------------------------
-- High-level interface
------------------------------------------------------------------------

-- | Run a given memory image as a list transducer.
intCodeToList ::
  [Integer] {- ^ initial memory -} ->
  [Integer] {- ^ inputs         -} ->
  [Integer] {- ^ outputs        -}
intCodeToList pgm = effectList (run (new pgm))

-- | Evaluate a program's effect as a function from a list of
-- inputs to a list of outputs.
effectList :: Effect -> [Integer] {- ^ inputs -} -> [Integer] {- ^ outputs -}
effectList effect inputs =
  case effect of
    Input f | x:xs <- inputs -> effectList (f x) xs
            | otherwise      -> error "Not enough inputs"
    Output o e               -> o : effectList e inputs
    Halt                     -> []
    Fault                    -> error "Bad instruction"

------------------------------------------------------------------------
-- Machine state
------------------------------------------------------------------------

-- | Machine state
data Machine = Machine
  { pc      :: !Integer               -- ^ program counter
  , relBase :: !Integer               -- ^ relative base pointer
  , memory  :: !(Map Integer Integer) -- ^ program memory
  }
  deriving (Eq, Ord, Show)

-- | Index memory at 0-based index
(!) :: Machine -> Integer -> Integer
m ! i = Map.findWithDefault 0 i (memory m)

-- | Construct memory from a list of initial values.
new :: [Integer] -> Machine
new initialValues = Machine
  { pc      = 0
  , relBase = 0
  , memory  = Map.fromList [ (k,v) | (k,v) <- zip [0..] initialValues, v /= 0]
  }

-- | Update the value stored at a given location in memory.
set ::
  Integer {- ^ position  -} ->
  Integer {- ^ new value -} ->
  Machine -> Machine
set i 0 m = m { memory = Map.delete i   (memory m) }
set i v m = m { memory = Map.insert i v (memory m) }

-- | Update the relative base pointer by adding an offset to it.
adjustRelBase :: Integer {- ^ offset -} -> Machine -> Machine
adjustRelBase i mach = mach { relBase = relBase mach + i }

-- | Set program counter to a new address.
jmp :: Integer -> Machine -> Machine
jmp i mach = mach { pc = i }

-- | Generate a list representation of memory starting from
-- zero. This can get big for sparsely filled memory using
-- large addresses.
memoryList :: Machine -> [Integer]
memoryList mach
  | Map.null (memory mach) = []
  | otherwise = map (mach !) [0 .. fst (Map.findMax (memory mach))]

------------------------------------------------------------------------
-- Parsing
------------------------------------------------------------------------

-- | Parse an Intcode program as a list of comma separated opcode integers.
memoryParser :: Parser [Integer]
memoryParser = number `sepBy` ","

------------------------------------------------------------------------
-- Big-step semantics
------------------------------------------------------------------------

-- | Possible effects from running a machine
data Effect
  = Output !Integer Effect    -- ^ Output an integer
  | Input (Integer -> Effect) -- ^ Input an integer
  | Halt                      -- ^ Halt execution
  | Fault                     -- ^ Execution failure
  deriving Show

-- | Big-step semantics of virtual machine.
run :: Machine -> Effect
run mach =
  case step mach of
    Step mach'        -> run mach'
    StepOut out mach' -> Output out (run mach')
    StepIn f          -> Input (run . f)
    StepHalt          -> Halt
    StepFault         -> Fault

-- | Compose two effects together. Outputs from first argument are
-- used as inputs to the second effect. Composed effect halts when
-- the second machine halts.
(>>>) :: Effect -> Effect -> Effect
x          >>> Output o y = Output o (x >>> y)
_          >>> Halt       = Halt
_          >>> Fault      = Fault
Output o x >>> Input f    = x >>> f o
Halt       >>> Input _    = Fault
Fault      >>> Input _    = Fault
Input f    >>> y          = Input (\i -> f i >>> y)

infixl 9 >>>

------------------------------------------------------------------------
-- Small-step semantics
------------------------------------------------------------------------

-- | Result of small-step semantics.
data Step
  = Step    !Machine             -- ^ no effect
  | StepOut !Integer !Machine    -- ^ output
  | StepIn  (Integer -> Machine) -- ^ input
  | StepHalt                     -- ^ halt
  | StepFault                    -- ^ bad instruction
  deriving Show

-- | Small-step semantics of virtual machine.
step :: Machine -> Step
step mach =
  case decode (at (pc mach)) of
    Nothing -> StepFault
    Just opcodeMode ->
      case mapWithIndex toPtr (pc mach + 1) $! opcodeMode of
        (pc', opcode) -> impl opcode $! jmp pc' mach

  where
    at :: Integer -> Integer
    at i = mach ! i

    toPtr :: Integer -> Mode -> Integer
    toPtr i Imm =    i
    toPtr i Abs = at i
    toPtr i Rel = at i + relBase mach

    impl opcode =
      case opcode of
        Add a b c -> Step . set c (at a + at b)
        Mul a b c -> Step . set c (at a * at b)
        Inp a     -> StepIn . flip (set a)
        Out a     -> StepOut (at a)
        Jnz a b   -> Step . if at a /= 0 then jmp (at b) else id
        Jz  a b   -> Step . if at a == 0 then jmp (at b) else id
        Lt  a b c -> Step . set c (if at a <  at b then 1 else 0)
        Eq  a b c -> Step . set c (if at a == at b then 1 else 0)
        Arb a     -> Step . adjustRelBase (at a)
        Hlt       -> const StepHalt

mapWithIndex :: (Integer -> a -> b) -> Integer -> Opcode a -> (Integer, Opcode b)
mapWithIndex f = mapAccumL (\i a -> (i+1, f i a))
{-# INLINE mapWithIndex #-}

------------------------------------------------------------------------
-- Opcode decoder
------------------------------------------------------------------------

-- | Parameter modes
data Mode
  = Abs -- ^ absolute position
  | Imm -- ^ immediate
  | Rel -- ^ relative position
  deriving (Eq, Ord, Read, Show)

-- | Opcodes parameterized over argument representations.
data Opcode a
  = Add !a !a !a -- ^ addition:        @c = a + b@
  | Mul !a !a !a -- ^ multiplication:  @c = a * b@
  | Inp !a       -- ^ input:           @a = input()@
  | Out !a       -- ^ output:          @output(a)@
  | Jnz !a !a    -- ^ jump-if-true:    @if a then goto b@
  | Jz  !a !a    -- ^ jump-if-false:   @if !a then goto b@
  | Lt  !a !a !a -- ^ less-than:       @c = a < b@
  | Eq  !a !a !a -- ^ equals:          @c = a == b@
  | Arb !a       -- ^ adjust-rel-base: @rel += a@
  | Hlt          -- ^ halt
  deriving (Eq, Ord, Read, Show, Functor, Foldable)

-- | Decode an instruction to determine the opcode and parameter modes.
--
-- >>> decode 1002
-- Just (Mul Abs Imm Abs)
decode :: Integer -> Maybe (Opcode Mode)
decode n = traverse par =<< opcode
  where
    par i =
      case digit (i+1) n of
        0 -> Just Abs
        1 -> Just Imm
        2 -> Just Rel
        _ -> Nothing

    opcode =
      case n `mod` 100 of
        _ | n < 0 -> Nothing
        1  -> Just (Add 1 2 3)
        2  -> Just (Mul 1 2 3)
        3  -> Just (Inp 1)
        4  -> Just (Out 1)
        5  -> Just (Jnz 1 2)
        6  -> Just (Jz  1 2)
        7  -> Just (Lt  1 2 3)
        8  -> Just (Eq  1 2 3)
        9  -> Just (Arb 1)
        99 -> Just Hlt
        _  -> Nothing

-- | Visits arguments from left to right.
instance Traversable Opcode where
  {-# INLINE traverse #-}
  traverse f o =
    case o of
      Add x y z -> Add <$> f x <*> f y <*> f z
      Mul x y z -> Mul <$> f x <*> f y <*> f z
      Inp x     -> Inp <$> f x
      Out x     -> Out <$> f x
      Jnz x y   -> Jnz <$> f x <*> f y
      Jz  x y   -> Jz  <$> f x <*> f y
      Lt  x y z -> Lt  <$> f x <*> f y <*> f z
      Eq  x y z -> Eq  <$> f x <*> f y <*> f z
      Arb x     -> Arb <$> f x
      Hlt       -> pure Hlt

-- | Extract the ith digit from a number.
--
-- >>> digit 0 2468
-- 8
-- >>> digit 3 2468
-- 2
-- >>> digit 4 2468
-- 0
digit :: Integer {- ^ position -} -> Integer {- ^ number -} -> Integer {- ^ digit -}
digit i x = x `div` (10^i) `mod` 10
