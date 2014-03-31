module Main (main, spec) where

import Test.Hspec
import Control.Arrow (left)
import Text.ParserCombinators.Parsec (Parser)

import Parser
import VM

main :: IO ()
main = hspec spec

parseTest :: Parser a -> String -> Either String a
parseTest p input = (left show $ parse p "Spec" input)

spec :: Spec
spec = do
        describe "instructionsParser" $ do
            context "with normalInstructionsString" $ do
                it "parses" $ do
                    parseTest instructionsParser normalInstructionsString `shouldBe`
                        (Right $ [Push 3, Add, Store 4])
            context "with commentBetweenLines" $ do
                it "parses" $ do
                    parseTest instructionsParser commentBetweenLines `shouldBe`
                        (Right $ [Push 5, Add])
        describe "runVM" $ do
            context "with normalInstructions" $ do
                it "runs" $ do
                    takeResult (runVM normalInstructions) `shouldBe` 3

normalInstructionsString :: String
normalInstructionsString = "Push 3\nAdd\nStore 4\n"

commentBetweenLines :: String
commentBetweenLines = "# aaa\nPush 5\n# Add\nAdd\n#Push 5"

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