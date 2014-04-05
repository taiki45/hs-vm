module VM
    ( Instruction (..)
    , runVM
    , takeResult
    , takePC ) where

import Control.Applicative ((<$>))
import Data.Monoid
import Data.Foldable (foldMap)

import VM.Machine
import VM.Instruction

runVM :: [Instruction] -> Machine
runVM is = run (instMorph <$> is) (setCounter 0 . prepare is $ initMachine)

run :: [Machine -> Machine] -> Machine -> Machine
run is m
    | end pc = is !! pc $ m
    | otherwise = run is (is !! pc $ m)
        where pc = fromInteger . takePC $ m
              end = (length is - 1 ==)

prepare :: [Instruction] -> Machine -> Machine
prepare is = appEndo . getDual $ foldMap (Dual . Endo . setLabel) is
