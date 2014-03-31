module ParserSpec where

import Test.Hspec
import Control.Arrow (left)
import Text.ParserCombinators.Parsec (Parser)

import Parser
import VM

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

parseTest :: Parser a -> String -> Either String a
parseTest p input = (left show $ parse p "Spec" input)

normalInstructionsString :: String
normalInstructionsString = "Push 3\nAdd\nStore 4\n"

commentBetweenLines :: String
commentBetweenLines = "# aaa\nPush 5\n# Add\nAdd\n#Push 5"
