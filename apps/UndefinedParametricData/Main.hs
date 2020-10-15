module Main where

import Data
import Example
import Parser
import Show
import Type

import Text.Megaparsec
import Text.Megaparsec.Char

data A a = A a deriving Show

parseWithUndefinedData :: Parser a -> Parser (A a)
parseWithUndefinedData parseSomething = do
  _ <- string "ABC"
  a <- parseSomething
  _ <- string "def"
  return $ A a



testVariablePosition = parseTest (string "VP<" :: Parser String)

main = do
  putStrLn $ "Parse with ParseSomeInt: ABC1def"
  print $ parse (parseWithUndefinedData parseSomeInt) "" "ABC1def"
  parseTest (parseWithUndefinedData parseSomeInt) "ABC1def"

  putStrLn $ "\n\nParse with ParseSomeInt: ABC2def"
  print $ parse (parseWithUndefinedData parseSomeInt) "" "ABC2def"
  parseTest (parseWithUndefinedData parseSomeInt) "ABC2def"

  putStrLn $ "\n\nParse with ParseSomeStr: ABCABCdef"
  print $ parse (parseWithUndefinedData parseSomeStr) "" "ABCABCdef"
  parseTest (parseWithUndefinedData parseSomeStr) "ABCABCdef"

  putStrLn $ "\n\nParse with ParseIIdx: ABCI[0]def"
  print $ parse (parseWithUndefinedData parseIIdx) "" "ABCI[0]def"
  parseTest (parseWithUndefinedData parseIIdx) "ABCI[0]def"

  putStrLn $ "\n\nParse with ParseSIdx: ABCS[\"DEF\"]def"
  print $ parse (parseWithUndefinedData parseSIdx) "" "ABCS[\"DEF\"]def"
  parseTest (parseWithUndefinedData parseSIdx) "ABCS[\"DEF\"]def"

  putStrLn $ "\n\nParse with ParseVP: ABCVP<HDD:=:S[\"DEF\"]>def"
  print $ parse (parseWithUndefinedData parseVP) "" "ABCVP<HDD:=:S[\"DEF\"]>def"
  parseTest (parseWithUndefinedData parseVP) "ABCVP<HDD:=:S[\"DEF\"]>def"
