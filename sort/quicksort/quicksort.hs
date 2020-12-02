quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) = quickSort firstHalf ++ [x] ++ quickSort secondHalf
    where firstHalf = filter (<=x) xs
          secondHalf = filter (>x) xs

list :: [Int]
list = [1, 4, 5, 1342, 44, 3, 4, 234, 77, 2354, 2, 1112, 11466, 100, 13, 41]

main :: IO()
main = print $ quickSort list
