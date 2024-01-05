module RandomGen (randomsIO) where

import Control.Arrow (first)
import System.Random (Random, getStdRandom, randoms, split)

randomsIO :: (Random a) => IO [a]
randomsIO = getStdRandom (first randoms . split)