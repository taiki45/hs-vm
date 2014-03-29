module Instruction
    ( Instruction
        ( Add
        , Sub
        , Store
        , Read
        , Push )
    , instMorph ) where

import Machine

-- Instruction
{- `3+4` compiled to:
--   Push 3
--   Push 4
--   Add
-}
data Instruction = Add -- Add memory value to register value
                 | Sub -- Subtract register value with memory value
                 | Store Adress -- Store register value to memory
                 | Read Adress -- Push memory value to register
                 | Push Value -- Push constant value to register

instMorph :: Instruction -> Machine -> Machine
instMorph Add m = appBinOp (+) `mapReg` m
instMorph Sub m = appBinOp (-) `mapReg` m
instMorph (Store i) m@(M r _) = updateMem i (fetch r) `mapMem` m
instMorph (Read i) m@(M _ mem) = (updateReg $ fetchMem i mem) `mapReg` m
instMorph (Push v) m = updateReg v `mapReg` m
