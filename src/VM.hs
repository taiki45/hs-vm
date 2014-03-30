module VM
    ( Instruction (..)
    , runVM
    , takeResult ) where

import Data.Monoid
import Data.Foldable (foldMap)

import VM.Machine
import VM.Instruction

takeResult :: Machine -> Value
takeResult (M reg _) = fetch reg

runVM :: [Instruction] -> Machine
runVM ins = appEndo (compose ins) initMachine
    where compose = foldMap (Endo . instMorph) . reverse