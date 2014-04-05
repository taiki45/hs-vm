module VM
    ( Instruction (..)
    , runVM
    , takeResult
    , takePC ) where

import Control.Applicative ((<$>))

import VM.Machine
import VM.Instruction

runVM :: [Instruction] -> Machine
runVM is = run (instMorph <$> is) initMachine

run :: [Machine -> Machine] -> Machine -> Machine
run is m
    | end pc = is !! pc $ m
    | otherwise = run is (is !! pc $ m)
        where pc = fromInteger . takePC $ m
              end = (length is - 1 ==)
