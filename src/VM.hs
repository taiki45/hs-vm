module VM
    ( Instruction (..)
    , runVM
    , takeResult ) where

import Data.Monoid
import Data.Foldable (foldMap)

import VM.Machine
import VM.Instruction

runVM :: [Instruction] -> Machine
runVM ins = (appEndo . getDual) (compose ins) initMachine
    where compose = foldMap (Dual . Endo . instMorph)
