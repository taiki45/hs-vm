module VM
    ( Instruction (..)
    , runVM
    , takeResult ) where

import Data.Monoid
import Data.Foldable (foldMap)

import VM.Machine
import VM.Instruction

takeResult :: Machine -> Value
takeResult (M ds _) = fetch ds

runVM :: [Instruction] -> Machine
runVM ins = (appEndo . getDual) (compose ins) initMachine
    where compose = foldMap (Dual . Endo . instMorph)
