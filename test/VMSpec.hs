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
                   , Push 0
                   , Store 0 -- a = 0
                   , Load 0
                   , Push 10
                   , Lt -- a < 10
                   , Not -- #pc5
                   , JumpIf "while end" -- first while condition
                   , Label "body"
                   , Load 0
                   , Push 1
                   , Add
                   , Store 0 -- a += 1 #pc10
                   , Load 0
                   , Push 10
                   , Lt
                   , JumpIf "body" -- second while condition
                   , Label "while end"
                   , Load 0] -- a will be 10 #pc15

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
                           , Store 0
                           , Pop
                           , Load 0
                           , Push 3 -- [3,4]
                           , Add -- [7]
                           , Ret
                           , Label "add_five"
                           , Store 0
                           , Pop
                           , Load 0
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
                  , Store 0 -- store local var
                  , Pop
                  , Load 0 -- set n
                  , Push 0
                  , Eq -- n == 0
                  , Not
                  , JumpIf "fib if2"
                  , Pop
                  , Push 0
                  , Ret -- return in first body
                  , Label "fib if2"
                  , Pop -- discard Eq result
                  , Load 0 -- set n
                  , Push 1
                  , Eq
                  , Not
                  , JumpIf "fib if3"
                  , Pop -- discard Eq result
                  , Push 1
                  , Ret -- return in second body
                  , Label "fib if3"
                  , Pop -- discard Eq result
                  , Load 0
                  , Push 1
                  , Sub
                  , Call "fib"
                  , Store 1 -- store fib(n - 1) result
                  , Pop
                  , Load 0
                  , Push 2
                  , Sub
                  , Call "fib"
                  , Store 2 -- store fib(n - 2) result
                  , Pop
                  , Load 1
                  , Load 2
                  , Add
                  , Ret
                  , Label "main"
                  , Push 10
                  , Call "fib"] -- shouldBe 55
