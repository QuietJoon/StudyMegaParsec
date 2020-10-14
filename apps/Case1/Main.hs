module Main where

import Data
import Example
import Parser
import Show
import Type

import Text.Megaparsec
import Text.Megaparsec.Char

parseVariablePosition :: String -> VP
parseVariablePosition = undefined

testVariablePosition = parseTest (string "VP<" :: Parser String)

main = do
  print "ABC"
  print vp1
  testVariablePosition $ show vp2
  testVariablePosition $ show hdd

  putStrLn $ "\n\nParseVP: " <> show vp1
  print $ parse parseVP "" (show vp1)
  parseTest parseVP (show vp1)

  putStrLn $ "\n\nParseVP: " <> show vp2
  parseTest parseVP (show vp2)

  putStrLn $ "\n\nParseVP: " <> show vp3
  print $ parse parseVP "" (show vp3)
  parseTest parseVP (show vp3)

  putStrLn $ "\n\nParseVP: " <> show vp4
  parseTest parseVP (show vp4)

  putStrLn $ "\n\nParseVP: " <> show vp5
  parseTest parseVP (show vp5)

  putStrLn $ "\n\nParseVP: " <> show hdd
  parseTest parseVP (show hdd)
