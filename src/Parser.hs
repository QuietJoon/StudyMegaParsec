module Parser where

import Data
import Parser.Type
import Parser.Util

import Control.Monad (void)
import Data.Maybe ( fromJust, isJust )
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

parseVP :: Parser VP
parseVP = do
  void (string "VP<")
  p <- parsePlace
  void (string ":=:")
  i <- parseIdx
  void (string ">")
  return $ VP p i

parsePlace :: Parser Place
parsePlace =
  choice
    [ HDD <$ string "HDD"
    , RAM <$ string "RAM"
    ]

parseIdx :: Parser Idx
parseIdx = try parseIIdx <|> parseSIdx

parseIIdx :: Parser Idx
parseIIdx = do
  void (string "I[")
  i <- parseSignedInt
  void (string "]")
  return $ IIdx i

parseIIdx' :: Parser Idx
parseIIdx' = do
  void (string "I[")
  i <- parseSomeInt
  void (string "]")
  return $ IIdx i

parseSomeInt :: Parser Int
parseSomeInt =
  choice
    [ 0 <$ string "0"
    , 1 <$ string "1"
    ]

parseSIdx :: Parser Idx
parseSIdx = do
  void (string "S[")
  s <- parseQuotedString
  void (string "]")
  return $ SIdx s

parseSIdx' :: Parser Idx
parseSIdx' = do
  void (string "S[\"")
  s <- parseSomeStr
  void (string "\"]")
  return $ SIdx s

parseSomeStr :: Parser String
parseSomeStr =
  choice
    [ "ABC" <$ string "ABC"
    , "DEF" <$ string "DEF"
    ]
