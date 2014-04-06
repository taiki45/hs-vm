{-# LANGUAGE TupleSections #-}
module VM.Machine
    ( Machine (..)
    , mapDS
    , mapMem
    , mapLabels
    , cup
    , setCounter
    , initMachine
    , takeResult
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

-- class Fetchable
-- class Updatable

-- Machine and functions
data Machine = M
                { takeDS :: DataStack
                , takeMem :: Mem
                , takePC :: PC
                , takeL :: Labels }
             deriving Show

mapDS :: (DataStack -> DataStack) -> Machine -> Machine
f `mapDS` (M r m c l) = M (f r) m c l

mapMem :: (Mem -> Mem) -> Machine -> Machine
f `mapMem` (M r m c l) = M r (f m) c l

mapLabels :: (Labels -> Labels) -> Machine -> Machine
f `mapLabels` (M r m c l) = M r m c (f l)

cup :: Machine -> Machine
cup (M r m c l) = M r m (countUp c) l

setCounter :: PC -> Machine -> Machine
setCounter c (M r m _ l) = M r m c l

-- initialize machine with empty value
initMachine :: Machine
initMachine = M initDataStack initMem initPC initLabels

takeResult :: Machine -> Value
takeResult m = fetch $ takeDS m


-- DataStack and functions
type DataStack = [Value]

push :: Value -> DataStack -> DataStack
push v s = v:s

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

countUp :: PC -> PC
countUp = (+1)

initPC :: PC
initPC = 0


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
