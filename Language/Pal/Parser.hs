module Language.Pal.Parser
  ( expr
  ) where

import Control.Applicative
import Text.Parsec.Char hiding (string)
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec ((<?>))

import Language.Pal.Types


list :: Parser LValue
list = char '(' *> (List <$> (expr `sepBy1` whitespaces)) <* char ')'

whitespace :: Parser Char
whitespace = oneOf " \n\t"

whitespaces :: Parser String
whitespaces = many1 whitespace

expr :: Parser LValue
expr =    Atom <$> atom
       <|> list
       <|> Number <$> number
       <|> String <$> string
       <|> Bool <$> bool
       <?> "expression"

atom :: Parser LAtom
atom = many1 letter

number :: Parser LNumber
number = read <$> many1 digit

string :: Parser LString
string = char '"' *> many (noneOf "\"") <* char '"'

bool :: Parser Bool
bool = char '#' *> ((char 't' *> pure True) <|> (char 'f' *> pure False))
