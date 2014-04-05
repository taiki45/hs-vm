module VM.Instruction
    ( Instruction (..)
    , instMorph ) where

import VM.Machine

-- Instruction
{- `3+4` compiled to:
--   Push 3
--   Push 4
--   Add
-}
data Instruction = Add -- Add data stack values
                 | Sub -- Subtract data stack values
                 | Lt -- Left data stack value is less than the right value
                 | Le -- left data stack value <= right data stack value
                 | Gt -- Left data stack value is greater than the right data stack value
                 | Ge -- left data stack value >= right data stack value
                 | Eq -- Equal left data stack value and right value
                 | Store Adress -- Store data stack value to memory
                 | Load Adress -- Push memory value to data stack
                 | Push Value -- Push constant value to data stack
                 deriving (Show, Read, Eq)

instMorph :: Instruction -> Machine -> Machine
instMorph Add m = appBinOp (+) `mapDS` m
instMorph Sub m = appBinOp (-) `mapDS` m
instMorph Lt m = appBinOp (boolToValue <.> (<))  `mapDS` m
instMorph Le m = appBinOp (boolToValue <.> (<=)) `mapDS` m
instMorph Gt m = appBinOp (boolToValue <.> (>))  `mapDS` m
instMorph Ge m = appBinOp (boolToValue <.> (>=)) `mapDS` m
instMorph Eq m = appBinOp (boolToValue <.> (==)) `mapDS` m
instMorph (Store i) m@(M r _) = updateMem i (fetch r) `mapMem` m
instMorph (Load i) m@(M _ mem) = push (fetchMem i mem) `mapDS` m
instMorph (Push v) m = push v `mapDS` m

boolToValue :: Bool -> Value
boolToValue False = 0
boolToValue True = 1

-- compose binary operator and function to binary operator
(<.>) :: (a -> Value) -> (Value -> Value -> a) -> Value -> Value -> Value
(<.>) f op v w = f $ op v w
