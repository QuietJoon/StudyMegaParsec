module Parser where

import Data
import Type

import Text.Megaparsec
import Text.Megaparsec.Char

parseVP :: Parser VP
parseVP = do
  _ <- string "VP<"
  p <- parsePlace
  _ <- string ":=:"
  i <- parseIdx
  _ <- string ">"
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
  _ <- string "I["
  i <- parseSomeInt
  _ <- string "]"
  return $ IIdx i

parseSomeInt :: Parser Int
parseSomeInt =
  choice
    [ 0 <$ string "0"
    , 1 <$ string "1"
    ]

parseSIdx :: Parser Idx
parseSIdx = do
  _ <- string "S[\""
  s <- parseSomeStr
  _ <- string "\"]"
  return $ SIdx s

parseSomeStr :: Parser String
parseSomeStr =
  choice
    [ "ABC" <$ string "ABC"
    , "DEF" <$ string "DEF"
    ]
