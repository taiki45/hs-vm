module VM.Instruction
    ( Instruction (..)
    , instMorph
    , setLabel ) where

import VM.Machine

-- TODO: add call and ret
-- Instruction
{- `3+4` compiled to:
--   Push 3
--   Push 4
--   Add
-}
data Instruction = Add -- Add data stack values.
                 | Sub -- Subtract data stack values.
                 | Lt -- first data stack value is less than the second value.
                 | Le -- first data stack value <= second data stack value.
                 | Gt -- first data stack value is greater than the second data stack value.
                 | Ge -- first data stack value >= second data stack value.
                 | Eq -- Equal left data stack value and second value.
                 | Not -- Turn over bool. non-zero goes 0, 0 goes 1.
                 | Store Adress -- Store data stack value to memory.
                 | Load Adress -- Push memory value to data stack.
                 | Push Value -- Push constant value to data stack.
                 | Label LabelName -- Set label and save current position.
                 | Jump LabelName -- Unconditional jump.
                 | JumpIf LabelName -- Jump if first stack is non-zero.
                 deriving (Show, Read, Eq)

-- Don't count up PC if jump instructions are given
instMorph :: Instruction -> Machine -> Machine
instMorph i@(Jump _) m = instMorph' i m
instMorph i@(JumpIf _) m = instMorph' i m
instMorph i m = cup $ instMorph' i m

setLabel :: Instruction -> Machine -> Machine
setLabel (Label n) m@(M _ _ c ls) = cup$ const (insertL n c ls) `mapLabels` m
setLabel _ m = cup m

instMorph' :: Instruction -> Machine -> Machine
instMorph' Add m = appBinOp (+) `mapDS` m
instMorph' Sub m = appBinOp (-) `mapDS` m
instMorph' Lt m = appBinOp (boolToValue <.> (<))  `mapDS` m
instMorph' Le m = appBinOp (boolToValue <.> (<=)) `mapDS` m
instMorph' Gt m = appBinOp (boolToValue <.> (>))  `mapDS` m
instMorph' Ge m = appBinOp (boolToValue <.> (>=)) `mapDS` m
instMorph' Eq m = appBinOp (boolToValue <.> (==)) `mapDS` m
instMorph' Not m = appF notOp `mapDS` m
instMorph' (Store i) m@(M r _ _ _) = updateMem i (fetch r) `mapMem` m
instMorph' (Load i) m@(M _ mem _ _) = push (fetchMem i mem) `mapDS` m
instMorph' (Push v) m = push v `mapDS` m
instMorph' (Label _) m = m
instMorph' (Jump n) m@(M _ _ _ ls) = setCounter (lookupL n ls) m
instMorph' (JumpIf n) m@(M ds _ _ ls)
    | fetch ds == 0 = cup m
    | otherwise     = setCounter (lookupL n ls) m

boolToValue :: Bool -> Value
boolToValue False = 0
boolToValue True = 1

notOp :: Value -> Value
notOp 0 = 1
notOp _ = 0

-- compose binary operator and function to binary operator
(<.>) :: (a -> Value) -> (Value -> Value -> a) -> Value -> Value -> Value
(<.>) f op v w = f $ op v w
