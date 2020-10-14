module Example where

import Data

vp1 = VP hdd idx0
vp2 = VP hdd idx1
vp3 = VP hdd idx2
vp4 = VP ram idxABC
vp5 = VP ram idxDEF

hdd = HDD
ram = RAM

idx0 = IIdx 0
idx1 = IIdx 1
idx2 = IIdx 2

idxABC = SIdx "ABC"
idxDEF = SIdx "DEF"
