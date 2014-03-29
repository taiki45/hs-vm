module VM
    ( Instruction
        ( Add
        , Sub
        , Store
        , Read
        , Push )
    , runVM
    , takeResult ) where

import Data.Monoid
import Data.Foldable (foldMap)

import Machine
import Instruction

takeResult :: Machine -> Value
takeResult (M reg _) = fetch reg

runVM :: [Instruction] -> Machine
runVM ins = appEndo (compose ins) initMachine
    where compose = foldMap (Endo . instMorph)
