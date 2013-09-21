module Language.Pal.Parser
  ( list
  ) where

import Control.Applicative
import Text.Parsec.Char hiding (string)
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec ((<?>))


type LList = [LValue]

data LValue = Atom LAtom
           | List LList
           | Number LNumber
           | String LString
           | Bool Bool

instance Show LValue where
  show (Atom a) = a
  show (List l) = '(' : unwords (map show l) ++ ")"
  show (Number n) = show n
  show (String s) = show s
  show (Bool True) = "#t"
  show (Bool False) = "#f"


type LAtom = String

type LNumber = Integer

type LString = String


list :: Parser LValue
list = char '(' *> (List <$> (value `sepBy1` whitespaces)) <* char ')'

whitespace :: Parser Char
whitespace = oneOf " \n\t"

whitespaces :: Parser String
whitespaces = many1 whitespace

value :: Parser LValue
value =     Atom <$> atom
        <|> list
        <|> Number <$> number
        <|> String <$> string
        <|> Bool <$> bool
        <?> "value"

atom :: Parser LAtom
atom = many1 letter

number :: Parser LNumber
number = read <$> many1 digit

string :: Parser LString
string = char '"' *> many (noneOf "\"") <* char '"'

bool :: Parser Bool
bool = char '#' *> ((char 't' *> pure True) <|> (char 'f' *> pure False))
