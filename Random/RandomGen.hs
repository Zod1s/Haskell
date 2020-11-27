module RandomGen (randomsIO) where

import System.Random (getStdRandom, randoms, Random, split)
import Control.Arrow (first)

randomsIO :: (Random a) => IO [a]
randomsIO = getStdRandom (first randoms . split)