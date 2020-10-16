module Main where

import Data
import Example
import Parser
import Parser.Type
import Show

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char

data A a = A a deriving (Show)


main = do
  putStrLn $ "\n\nParse with parseQuotedString: \"ABCABCdef\""
  print $ parse parseQuotedString "" "\"ABCABCdef\""
  parseTest parseQuotedString "\"ABCABCdef\""

  putStrLn $ "\n\nParse with parseNonQuotedString: ABCABCdef"
  print $ parse parseNonQuotedString "" "ABCABCdef"
  parseTest parseNonQuotedString "ABCABCdef"

  putStrLn $ "\n\nParse with parseSIdx': S[\"DEF\"]def"
  print $ parse parseSIdx' "" "S[\"DEF\"]def"
  parseTest parseSIdx' "S[\"DEF\"]def"
