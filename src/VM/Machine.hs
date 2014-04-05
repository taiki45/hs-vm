{-# LANGUAGE TupleSections #-}
module VM.Machine
    ( Machine (M)
    , mapDS
    , mapMem
    , cup
    , setCounter
    , initMachine
    , takeResult
    , takePC
    , DataStack
    , push
    , fetch
    , appBinOp
    , appF
    , initDataStack
    , Mem
    , Adress
    , updateMem
    , fetchMem
    , initMem
    , PC
    , Value) where

import Control.Applicative hiding (empty)
import Data.Array
import Data.Tuple (swap)

-- class Fetchable
-- class Updatable

-- Machine and functions
data Machine = M DataStack Mem PC
             deriving Show

mapDS :: (DataStack -> DataStack) -> Machine -> Machine
f `mapDS` (M r m c) = M (f r) m c

mapMem :: (Mem -> Mem) -> Machine -> Machine
f `mapMem` (M r m c) = M r (f m) c

cup :: Machine -> Machine
cup (M r m c) = M r m (countUp c)

setCounter :: PC -> Machine -> Machine
setCounter c (M r m _) = M r m c

-- initialize machine with empty value
initMachine :: Machine
initMachine = M initDataStack initMem initPC

takeResult :: Machine -> Value
takeResult (M ds _ _) = fetch ds

takePC :: Machine -> PC
takePC (M _ _ c) = c


-- DataStack and functions
type DataStack = (Value,Value)

push :: Value -> DataStack -> DataStack
push v (_,z) = (z,v)

fetch :: DataStack -> Value
fetch (a,_) = a

appBinOp :: (Value -> Value -> Value) -> DataStack -> DataStack
appBinOp op (v,w) = (op v w, empty)

appF :: (Value -> Value) -> DataStack -> DataStack
appF = fstmap

fstmap :: (a -> c) -> (a,b) -> (c,b)
fstmap f = swap . fmap f . swap

initDataStack :: DataStack
initDataStack = (empty,empty)


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


-- Program counter
type PC = Integer

countUp :: PC -> PC
countUp = (+1)

initPC :: PC
initPC = 0


-- Value
type Value = Integer

empty :: Value
empty = 0
