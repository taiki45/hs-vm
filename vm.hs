{-# LANGUAGE TupleSections #-}
import Control.Applicative hiding (empty)
--import Control.Monad
--import Control.Monad.State
import Data.Array
import Data.Tuple (swap)
import Data.Monoid
import Data.Foldable (foldMap)

-- class Fetchable
-- class Updatable

-- Machine and functions
data Machine = M Register Mem

mapReg :: (Register -> Register) -> Machine -> Machine
f `mapReg` (M r m) = M (f r) m

mapMem :: (Mem -> Mem) -> Machine -> Machine
f `mapMem` (M r m) = M r (f m)


-- Register and functions
type Register = (Integer,Integer)

updateReg :: Value -> Register -> Register
updateReg v (_,z) = (v,z)

fetch :: Register -> Value
fetch (a,_) = a

appBinOp :: (Value -> Value -> Value) -> Register -> Register
appBinOp op (v,w) = (op v w, empty)

initRegister :: Register
initRegister = (empty,empty)


-- Memory and functions
type Mem = Array Adress Value
type Adress = Integer

updateMem :: Adress -> Value -> Mem -> Mem
updateMem i v = flip (//) [(i,v)]

fetchMem :: Adress -> Mem -> Value
fetchMem i m = m ! i

initMem :: Mem
initMem = (array (0,initMemSize) (initTuple <$> [0..initMemSize]))
        where initTuple = swap . (empty,)


-- Value
type Value = Integer

empty :: Value
empty = 0

-- Instruction
{- `3+4` compiled to:
--   Push 3
--   Push 4
--   Add
-}
data Instruction = Add -- Add memory value to register value
                 | Sub -- Subtract register value with memory value
                 | Store Adress -- Store register value to memory
                 | Read Adress -- Push memory value to register
                 | Push Value -- Push constant value to register

instMorph :: Instruction -> Machine -> Machine
instMorph Add m = appBinOp (+) `mapReg` m
instMorph Sub m = appBinOp (-) `mapReg` m
instMorph (Store i) m@(M r _) = updateMem i (fetch r) `mapMem` m
instMorph (Read i) m@(M _ mem) = (updateReg $ fetchMem i mem) `mapReg` m
instMorph (Push v) m = updateReg v `mapReg` m


-- Runtime
main :: IO ()
main = putStrLn .show . takeResult $ machine
    where machine = runVM initMachine instructions

takeResult :: Machine -> Value
takeResult (M reg _) = fetch reg

runVM :: Machine -> [Instruction] -> Machine
runVM m ins = appEndo (compose ins) m
    where compose = foldMap (Endo . instMorph)

initMemSize :: Integer
initMemSize = 100

-- initialize machine with empty value
initMachine :: Machine
initMachine = M initRegister initMem

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
