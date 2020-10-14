module Show where

import Data

instance Show VP where
  show (VP p i) = "VP<" <> show p <> ":=:" <> show i <> ">"

instance Show Place where
  show HDD = "HDD"
  show RAM = "RAM"

instance Show Idx where
  show (IIdx i) = "I[" <> show i <> "]"
  show (SIdx s) = "S[" <> show s <> "]"
