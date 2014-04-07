{-# LANGUAGE TupleSections #-}
module VM.Machine
    ( Machine (..)
    , mapDS
    , mapMem
    , mapLabels
    , cup
    , setCounter
    , pushCS
    , popCS
    , initMachine
    , takeResult
    , DataStack
    , push
    , pop
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
    , inc
    , LabelName
    , Labels
    , insertL
    , lookupL
    , Value) where

import Control.Applicative hiding (empty)
import Data.Array
import qualified Data.Map as M
import Data.Maybe
import Data.Tuple (swap)


-- Machine and functions
data Machine = M { takeDS :: DataStack
                 , takeMem :: Mem
                 , takePC :: PC
                 , takeCS :: CS
                 , takeAS :: ArgStack
                 , takeL :: Labels }
             deriving Show

mapDS :: (DataStack -> DataStack) -> Machine -> Machine
f `mapDS` (M ds m c cs as l) = M (f ds) m c cs as l

mapMem :: (Mem -> Mem) -> Machine -> Machine
f `mapMem` (M ds m c cs as l) = M ds (f m) c cs as l

mapLabels :: (Labels -> Labels) -> Machine -> Machine
f `mapLabels` (M ds m c cs as l) = M ds m c cs as (f l)

cup :: Machine -> Machine
cup (M ds m c cs as l) = M ds m (inc c) cs as l

setCounter :: PC -> Machine -> Machine
setCounter c (M ds m _ cs as l) = M ds m c cs as l

pushCS :: PC -> Machine -> Machine
pushCS c (M ds m pc cs as l) = M ds initMem pc ((c,m):cs) as l

popCS :: Machine -> (PC, Machine)
popCS (M ds _ pc ((c,m):cs) as l) = (c,M ds m pc cs as l)

-- initialize machine with empty value
initMachine :: Machine
initMachine = M initDataStack initMem initPC initCS initAS initLabels

takeResult :: Machine -> Value
takeResult m = fetch $ takeDS m


-- DataStack and functions
type DataStack = [Value]

push :: Value -> DataStack -> DataStack
push v s = v:s

pop :: DataStack -> DataStack
pop (_:xs) = xs
pop _ = error "in pop"

fetch :: DataStack -> Value
fetch (v:_) = v

appBinOp :: (Value -> Value -> Value) -> DataStack -> DataStack
appBinOp op (w:v:xs) = op v w : xs

appF :: (Value -> Value) -> DataStack -> DataStack
appF = fstmap

fstmap :: (a -> a) -> [a] -> [a]
fstmap f (x:xs) = f x : xs

initDataStack :: DataStack
initDataStack = []


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

inc :: PC -> PC
inc = (+1)

initPC :: PC
initPC = 0


-- Call stack
type CS = [(PC, Mem)]

initCS :: CS
initCS = []

-- Argument stack
type ArgStack = [Value]

initAS :: ArgStack
initAS = []


-- Label set
type LabelName = String
type Labels = M.Map LabelName PC

initLabels :: M.Map LabelName PC
initLabels = M.fromList []

insertL :: LabelName -> PC -> Labels -> Labels
insertL = M.insert

lookupL :: LabelName -> Labels -> PC
lookupL n l = fromJust $ M.lookup n l


-- Value
type Value = Integer

empty :: Value
empty = 0
