module VM
    ( Instruction (..)
    , runVM
    , takeResult
    , takePC ) where

import Control.Applicative ((<$>))
import Data.Array
import Data.Monoid
import Data.Foldable (foldMap)

import VM.Machine
import VM.Instruction

runVM :: [Instruction] -> Machine
runVM is = run (toArray $ instMorph <$> is) (setCounter 0 . prepare is $ initMachine)

run :: Array PC (Machine -> Machine) -> Machine -> Machine
run is m | end pc = is ! pc $ m
         | otherwise = run is (is ! pc $ m)
        where pc = fromInteger . takePC $ m
              end = ((snd . bounds) is - 1 ==)

prepare :: [Instruction] -> Machine -> Machine
prepare is = appEndo . getDual $ foldMap (Dual . Endo . setLabel) is

toArray :: [a] -> Array PC a
toArray ls = listArray (0,toInteger $ length ls) ls
