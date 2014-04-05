module ParserSpec where

import Test.Hspec
import Control.Arrow (left)
import Text.ParserCombinators.Parsec (Parser)

import Parser
import VM

spec :: Spec
spec = describe "instructionsParser" $ do
            context "with normalInstructionsString" $
                it "parses" $
                    parseTest instructionsParser normalInstructionsString `shouldBe`
                        Right [Push 3, Add, Store 4]
            context "with commentBetweenLines" $
                it "parses" $
                    parseTest instructionsParser commentBetweenLines `shouldBe`
                        Right [Push 5, Add]
            context "with labelInstructions" $
                it "parsers" $
                    parseTest instructionsParser labelInstructions `shouldBe`
                        Right [Label "abc", Push 3, JumpIf "abc"]

parseTest :: Parser a -> String -> Either String a
parseTest p input = left show $ parse p "Spec" input

normalInstructionsString :: String
normalInstructionsString = "Push 3\nAdd\nStore 4\n"

commentBetweenLines :: String
commentBetweenLines = "# aaa\nPush 5\n# Add\nAdd\n#Push 5"

labelInstructions :: String
labelInstructions = "Label abc\nPush 3\nJumpIf abc"
