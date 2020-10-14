module Data where

data VP = VP Place Idx
data Place = HDD | RAM
data Idx = IIdx Int | SIdx String
