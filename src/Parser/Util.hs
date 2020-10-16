module Parser.Util where

import Parser.Type

import Control.Monad (void)
import Data.Maybe ( fromJust, isJust )
import Text.Megaparsec
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as L

lexeme :: Parser a -> Parser a
lexeme = L.lexeme Char.space

parseInt :: Parser Int
parseInt = lexeme L.decimal

parseSignedInt :: Parser Int
parseSignedInt = L.signed Char.space parseInt


parseChar :: Parser (Maybe Char)
parseChar = satisfy (/= '\"') >>= \c -> return $ Just c

parseQuotedString :: Parser String
parseQuotedString = do
  void (Char.char '\"')
  xs <- many parseChar
  let cs = map fromJust $ filter isJust xs
  void (Char.char '\"')
  return cs

parseNonQuotedString :: Parser String
parseNonQuotedString = do
  xs <- many parseChar
  let cs = map fromJust $ filter isJust xs
  return cs
