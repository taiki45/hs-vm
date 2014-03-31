module Main (main, spec) where

import Test.Hspec
import Control.Arrow (left)
import Text.ParserCombinators.Parsec (Parser)

import Parser
import VM.Instruction

main :: IO ()
main = hspec spec

parseTest :: Parser a -> String -> Either String a
parseTest p input = (left show $ parse p "Spec" input)

spec :: Spec
spec = do
        describe "instructionsParser" $ do
            context "with normalInstructions" $ do
                it "parses" $ do
                    parseTest instructionsParser normalInstructions `shouldBe`
                        (Right $ [Push 3, Add, Store 4])

normalInstructions :: String
normalInstructions = "Push 3\nAdd\nStore 4\n"
