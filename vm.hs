{-# LANGUAGE TupleSections #-}
import Control.Applicative
--import Control.Monad
--import Control.Monad.State
import Data.Array
import Data.Tuple (swap)
import Data.Monoid
import Data.Foldable (foldMap)

data Machine = M Register Mem

type Register = (Integer,Integer)
type Mem = Array Adress Value
type Adress = Integer
type Value = Integer

mapReg :: (Register -> Register) -> Machine -> Machine
f `mapReg` (M r m) = M (f r) m

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

instArrow :: Instruction -> Machine -> Machine
instArrow Add m = addReg `mapReg` m
instArrow Sub m = undefined
instArrow (Store _) m = undefined
instArrow (Read _) m = undefined
instArrow (Push _) m = undefined

addReg :: Register -> Register
addReg (x,y) = (x + y,0)

-- Runtime
main :: IO ()
main = putStrLn .show . takeResult $ machine
    where machine = runVM initMachine instructions

takeResult :: Machine -> Integer
takeResult (M reg m) = fetch reg
    where fetch (r,_) = r

runVM :: Machine -> [Instruction] -> Machine
runVM m ins = appEndo (compose ins) m
    where compose = foldMap (Endo . instArrow)

initMemSize :: Integer
initMemSize = 100

initMachine :: Machine
initMachine = M (0,0) (array (0,initMemSize) (initTuple <$> [0..initMemSize]))
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
