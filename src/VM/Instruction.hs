module VM.Instruction
    ( Instruction (..)
    , instMorph
    , setLabel ) where

import VM.Machine

-- Instruction
{- `3+4` compiled to:
--   Push 3
--   Push 4
--   Add
-}
data Instruction
    = Add -- Add data stack values.
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
    | Pop -- Pop and discard value from data stack.
    | Dup -- Duplicate top value in data stack.
    | PushLocal -- Push local data stack.
    | PopLocal -- Pop local data stack.
    | Label LabelName -- Set label and save current position.
    | Jump LabelName -- Unconditional jump.
    | JumpIf LabelName -- Jump if first stack is non-zero. Then discatd value.
    | Call LabelName -- Store current PC and jump to function.
    | Ret -- Return from calling point.
    deriving (Show, Read, Eq)

-- Don't count up PC if jump instructions are given
instMorph :: Instruction -> Machine -> Machine
instMorph i@(Jump _) m = instMorph' i m
instMorph i@(JumpIf _) m = instMorph' i m
instMorph i@(Call _) m = instMorph' i m
instMorph Ret m = instMorph' Ret m
instMorph i m = cup $ instMorph' i m

setLabel :: Instruction -> Machine -> Machine
setLabel (Label n) m = cup$ const (insertL n c ls) `mapLabels` m
    where c  = takePC m
          ls = takeL m
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
instMorph' (Store i) m = updateMem i (fetch $ takeDS m) `mapMem` m
instMorph' (Load i) m = push (fetchMem i $ takeMem m) `mapDS` m
instMorph' (Push v) m = push v `mapDS` m
instMorph' Pop m = pop `mapDS` m
instMorph' Dup m = push (fetch $ takeDS m) `mapDS` m
instMorph' PushLocal m = pop `mapDS` (pushL (fetch $ takeDS m) `mapLDS` m)
instMorph' PopLocal m = const lds `mapLDS` (push v `mapDS` m)
                where (v, lds) = popL $ takeLDS m
instMorph' (Label _) m = m
instMorph' (Jump n) m = setCounter (lookupL n $ takeL m) m
instMorph' (JumpIf n) m
    | fetch (takeDS m) == 0 = cup poped
    | otherwise             = setCounter (lookupL n $ takeL m) poped
    where poped = pop `mapDS` m -- pop condition value
instMorph' (Call n) m = setCounter target . pushCS current $ m
        where current = takePC m
              target = lookupL n $ takeL m
instMorph' Ret m = setCounter (inc callpoint) m'
        where (callpoint, m') = popCS m

boolToValue :: Bool -> Value
boolToValue False = 0
boolToValue True = 1

notOp :: Value -> Value
notOp 0 = 1
notOp _ = 0

-- compose binary operator and function to binary operator
(<.>) :: (a -> Value) -> (Value -> Value -> a) -> Value -> Value -> Value
(<.>) f op v w = f $ op v w
