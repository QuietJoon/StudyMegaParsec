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


parseChar' :: Parser (Maybe Char)
parseChar' = satisfy (/= '\"') >>= \c -> return $ Just c

parseQuotedString' :: Parser String
parseQuotedString' = do
  void (Char.char '\"')
  xs <- many parseChar'
  let cs = map fromJust $ filter isJust xs
  void (Char.char '\"')
  return cs

parseNonQuotedString' :: Parser String
parseNonQuotedString' = do
  xs <- many parseChar'
  let cs = map fromJust $ filter isJust xs
  return cs


-- Get from https://stackoverflow.com/questions/52319817/parse-between-quotes-with-haskell

escape :: Parser String
escape = do
    d <- Char.char '\\'
    c <- oneOf ['\\', '\"', '0', 'n', 'r', 'v', 't', 'b', 'f']
    return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf ['\\', '\"', '\0', '\n', '\r', '\v', '\t', '\b', '\f']

parseQuotedString :: Parser String
parseQuotedString =
    let inner = fmap return (try nonEscape) <|> escape
    in  do
      Char.char '"'
      strings <- many inner
      Char.char '"'
      return $ concat strings

parseNonQuotedString :: Parser String
parseNonQuotedString =
    let inner = fmap return (try nonEscape) <|> escape
    in  do
      strings <- many inner
      return $ concat strings
