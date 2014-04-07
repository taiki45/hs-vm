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
                                       , pushl
                                       , popl
                                       , push
                                       , pop
                                       , dup
                                       , label
                                       , jumpIf
                                       , jump
                                       , call
                                       , ret ])

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

pop :: Parser Instruction
pop = Pop <$ string "Pop"

dup :: Parser Instruction
dup = Dup <$ string "Dup"

pushl :: Parser Instruction
pushl = PushLocal <$ string "PushLocal"

popl :: Parser Instruction
popl = PopLocal <$ string "PopLocal"

label :: Parser Instruction
label = Label <$> instWithString "Label"

jump :: Parser Instruction
jump = Jump <$> instWithString "Jump"

jumpIf :: Parser Instruction
jumpIf = JumpIf <$> instWithString "JumpIf"

call :: Parser Instruction
call = Call <$> instWithString "Call"

ret :: Parser Instruction
ret = Ret <$ string "Ret"

instWithNumber :: String ->  Parser Integer
instWithNumber s = read <$> (string s >> spaces >> many1 digit)

instWithString :: String -> Parser String
instWithString s = string s
                    >> spaces
                    >> many1 (noneOf "\n")

comment :: Parser (Maybe Instruction)
comment = char '#' >> many (noneOf "\n") >> return Nothing
