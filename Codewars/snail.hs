import Data.List

snail :: [[Int]] -> [Int]
snail [] = []
snail [[]] = []
snail (x : xs) = x ++ snail (rotl xs)
  where
    rotl = reverse . transpose
