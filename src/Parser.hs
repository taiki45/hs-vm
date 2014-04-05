module Parser
    ( parse
    , instructionsParser ) where

import Control.Applicative ((<$>), (<$))
import Data.Maybe (catMaybes)
import Text.ParserCombinators.Parsec hiding (label)
import VM.Instruction

instructionsParser :: Parser [Instruction]
instructionsParser = catMaybes <$> (comment <|> instruction <|> blank) `sepBy` many1 newline
                where blank = Nothing <$ eof

instruction :: Parser (Maybe Instruction)
instruction = Just <$> choice (try <$> [ add
                                       , sub
                                       , lt
                                       , le
                                       , gt
                                       , ge
                                       , eq
                                       , not'
                                       , store
                                       , load
                                       , push
                                       , label
                                       , jump
                                       , jumpIf ])

add :: Parser Instruction
add = Add <$ string "Add"

sub :: Parser Instruction
sub = Sub <$ string "Sub"

lt :: Parser Instruction
lt = Lt <$ string "Lt"

le :: Parser Instruction
le = Le <$ string "Le"

gt :: Parser Instruction
gt = Gt <$ string "Gt"

ge :: Parser Instruction
ge = Ge <$ string "Ge"

eq :: Parser Instruction
eq = Eq <$ string "Eq"

not' :: Parser Instruction
not' = Not <$ string "Not"

store :: Parser Instruction
store = Store <$> instWithNumber "Store"

load :: Parser Instruction
load = Load <$> instWithNumber "Load"

push :: Parser Instruction
push = Push <$> instWithNumber "Push"

label :: Parser Instruction
label = Label <$> instWithString "Label"

jump :: Parser Instruction
jump = Jump <$> instWithString "Jump"

jumpIf :: Parser Instruction
jumpIf = JumpIf <$> instWithString "JumpIf"

instWithNumber :: String ->  Parser Integer
instWithNumber s = read <$> (string s >> spaces >> many1 digit)

instWithString :: String -> Parser String
instWithString s = string s >> spaces >> many (try $ noneOf "\n")

comment :: Parser (Maybe Instruction)
comment = char '#' >> many (noneOf "\n") >> return Nothing
