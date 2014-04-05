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


simpleInstructions :: [Instruction]
simpleInstructions = [ Push 3
                     , Push 4]

-- test instructions for:
--   a = 3 + 4
--   b = a + 3
--   b - a
normalInstructions :: [Instruction]
normalInstructions = [ Push 3
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
jumpInstructions = [ Push 0
                   , Store 0 -- a = 0
                   , Load 0
                   , Push 10
                   , Lt -- a < 10
                   , Not -- #pc5
                   , JumpIf 16 -- first while condition
                   , Load 0
                   , Push 1
                   , Add
                   , Store 0 -- a += 1 #pc10
                   , Load 0
                   , Push 10
                   , Lt
                   , JumpIf 7 -- second while condition
                   , Load 0] -- a will be 10 #pc15
