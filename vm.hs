{-# LANGUAGE TupleSections #-}
import Control.Applicative
--import Control.Monad
--import Control.Monad.State
import Data.Array
import Data.Tuple (swap)
import Data.Monoid

fstmap :: (a -> c) -> (a,b) -> (c,b)
fstmap f = swap . fmap f . swap

data Machine = M { registerValue :: Register
                 , memValue      :: Mem      }

type Register = (Integer,Integer)
rmap :: (Value -> Value) -> Machine -> Machine
f `rmap` (M reg mem) = M (fstmap f reg) mem

type Mem = Array Adress Value
type Adress = Integer
type Value = Integer

{- `3+4` compiled to:
--   Push 3
--   Push 4
--   Add
-}
data Instruction = Add -- Add memory value to register value
                 | Sub -- Subtract register value with memory value
                 | Store Adress -- Store register value to memory
                 | Read Adress -- Push memory value to register
                 | Push Integer -- Push constant value to register
                 | Id

instArrow :: Instruction -> Machine -> Machine
instArrow Add (M r m) = undefined
instArrow Sub (M r m) = undefined
instArrow (Store i) (M r m) = undefined
instArrow (Read i) (M r m) = undefined
instArrow (Push c) (M r m) = undefined
instArrow Id m = m

main = undefined

runVM = undefined

initMemSize = 100

init :: Machine
init = M (0,0) (array (0,initMemSize) (initTuple <$> [0..initMemSize]))
    where initTuple = swap . (0,)

{- test instructions for:
-    a = 3 + 4
--   b = a + 3
--   b - a
-}
instructions :: [Instruction]
instructions = [ Push 3
               , Push 4
               , Add
               , Store 0 -- end first line
               , Read 0
               , Push 3
               , Add
               , Sub ]
