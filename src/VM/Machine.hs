{-# LANGUAGE TupleSections #-}
module VM.Machine
    ( Machine (M)
    , mapReg
    , mapMem
    , initMachine
    , Register
    , updateReg
    , fetch
    , appBinOp
    , appF
    , initRegister
    , Mem
    , Adress
    , updateMem
    , fetchMem
    , initMem
    , Value) where

import Control.Applicative hiding (empty)
import Data.Array
import Data.Tuple (swap)

-- class Fetchable
-- class Updatable

-- Machine and functions
data Machine = M Register Mem
             deriving Show

mapReg :: (Register -> Register) -> Machine -> Machine
f `mapReg` (M r m) = M (f r) m

mapMem :: (Mem -> Mem) -> Machine -> Machine
f `mapMem` (M r m) = M r (f m)

-- initialize machine with empty value
initMachine :: Machine
initMachine = M initRegister initMem


-- Register and functions
type Register = (Value,Value)

updateReg :: Value -> Register -> Register
updateReg v (_,z) = (z,v)

fetch :: Register -> Value
fetch (a,_) = a

appBinOp :: (Value -> Value -> Value) -> Register -> Register
appBinOp op (v,w) = (op v w, empty)

appF :: (Value -> Value) -> Register -> Register
appF = fstmap

fstmap :: (a -> c) -> (a,b) -> (c,b)
fstmap f = swap . fmap f . swap

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
initMem = array (0,initMemSize) (initTuple <$> [0..initMemSize])
        where initTuple = swap . (empty,)

initMemSize :: Integer
initMemSize = 100


-- Value
type Value = Integer

empty :: Value
empty = 0
