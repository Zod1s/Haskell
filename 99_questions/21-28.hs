import Data.List

-- | Problem 21
insertAt :: Int -> Char -> String -> String
insertAt i c s = first ++ [c] ++ second
    where (first, second) = splitAt i s 

-- | Problem 22
range :: Int -> Int -> [Int]
range s f = [s..f]

main :: IO()
main = print $ range 3 4