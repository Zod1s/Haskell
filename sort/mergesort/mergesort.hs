mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort first) (mergeSort second)
  where
    half = div (length xs) 2
    (first, second) = splitAt half xs

merge :: (Ord a) => [a] -> [a] -> [a]
merge l [] = l
merge [] r = r
merge l@(x : xs) r@(y : ys)
  | x <= y = x : merge xs r
  | otherwise = y : merge l ys

list :: [Int]
list = [1, 435, 123, 556, 334, 12, 335, 7743, 2134, 2]

main :: IO ()
main = print $ mergeSort list