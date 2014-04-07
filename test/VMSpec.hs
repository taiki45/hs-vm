module VMSpec where

import Test.Hspec

import VM

spec :: Spec
spec = describe "runVM" $ do
            context "with simpleInstructions" $
                it "runs" $
                    takeResult (runVM simpleInstructions) `shouldBe` 4
            context "with normalInstructions" $ do
                it "runs" $
                    takeResult (runVM normalInstructions) `shouldBe` 3
                it "counts up PC" $
                    fromInteger (takePC (runVM normalInstructions)) `shouldBe` length normalInstructions
            context "with jumpInstructions" $ do
                it "runs" $
                    takeResult (runVM jumpInstructions) `shouldBe` 10
                it "set and counts up PC" $
                    fromInteger (takePC (runVM jumpInstructions)) `shouldBe` length jumpInstructions
            context "with functionCallInstructions" $
                it "runs" $
                    takeResult (runVM functionCallInstructions) `shouldBe` 12
            context "with fibInstructions" $
                it "runs" $
                    takeResult (runVM fibInstructions) `shouldBe` 55


simpleInstructions :: [Instruction]
simpleInstructions = [ Label "main"
                     , Push 3
                     , Push 4]

-- test instructions for:
--   a = 3 + 4
--   b = a + 3
--   b - a
normalInstructions :: [Instruction]
normalInstructions = [ Label "main"
                     , Push 3
                     , Push 4
                     , Add
                     , Store 0
                     , Load 0
                     , Push 3
                     , Add
                     , Store 1
                     , Load 1
                     , Load 0
                     , Sub ]

-- a = 0
-- while a < 10
--   a += 1
-- a
jumpInstructions :: [Instruction]
jumpInstructions = [ Label "main"
                   , Push 0 -- [0]
                   , Label "while start"
                   , Dup -- [0,0]
                   , Push 10 -- [10,0,0]
                   , Lt -- a < 10
                   , Not -- [1,0]
                   , JumpIf "while end" -- [0]
                   , Push 1 -- [1,0]
                   , Add -- [1]
                   , Jump "while start"
                   , Label "while end" ] -- a will be 10

-- def add_three(x)
--   x + 3
--
-- def add_five(x)
--   x + 5
--
-- def main
--   add_five(add_three(4))
functionCallInstructions :: [Instruction]
functionCallInstructions = [ Label "add_three"
                           , Push 3 -- [3,4]
                           , Add -- [7]
                           , Ret
                           , Label "add_five"
                           , Push 5 -- [5,7]
                           , Add -- [12]
                           , Ret
                           , Label "main"
                           , Push 4 -- [4]
                           , Call "add_three"
                           , Call "add_five"] -- shouldBe [12]

-- def fib(n)
--   if n == 0
--     0
--   elsif n == 1
--     1
--   else
--     fib(n - 1) + fib(n - 2)
--
-- def main
--   fib(10) -- shouldBe 55
fibInstructions :: [Instruction]
fibInstructions = [ Label "fib"
                  , Dup -- [10,10]
                  , Push 0 -- [0,10,10]
                  , Eq -- n == 0 -- [0,10]
                  , Not -- [1,10]
                  , JumpIf "fib if2" -- [10]
                  , Pop
                  , Push 0 -- [0]
                  , Ret -- return in first body
                  , Label "fib if2" -- [10]
                  , Dup -- [10,10]
                  , Push 1 -- [1,10,10]
                  , Eq -- [0,10]
                  , Not -- [1,10]
                  , JumpIf "fib if3" -- [10]
                  , Pop
                  , Push 1 -- [1]
                  , Ret -- return in second body
                  , Label "fib if3" -- [10]
                  , Dup -- [10,10]
                  , Push 1 -- [1,10,10]
                  , Sub -- [9,10]
                  , Call "fib" -- back with [?,10]
                  , PushLocal -- [10] and [?]
                  , Push 2 -- [2,10]
                  , Sub -- [8]
                  , Call "fib" -- back with [?]
                  , PopLocal -- -- [?,?]
                  , Add -- [55]
                  , Ret
                  , Label "main"
                  , Push 10
                  , Call "fib"] -- shouldBe 55
