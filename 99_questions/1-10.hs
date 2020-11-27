import qualified Data.List as L

data NestedList a = Elem a
                  | List [NestedList a]

-- | Problem 1
myLast :: [a] -> a
myLast [] = []
myLast [x] = x
myLast (_:xs) = myLast xs

-- | Problem 2
lastButOne :: [a] -> a
lastButOne = last . init

-- | Problem 3
kElem :: Int -> [a] -> a
kElem _ [] = error "Lista vuota"
kElem index xs 
    |index > length xs || index < 1 = error "Indice fuori dal limite"
    |otherwise = last . take index $ xs

-- | Problem 4
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

-- | Problem 5
reverse' :: [a] -> [a]
reverse' [x] = [x]
reverse' (x:xs) = (reverse' xs) ++ [x]

-- | Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse' xs

-- | Problem 7
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

-- | Problem 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:ys@(y:_))
    | x == y = compress ys
    | otherwise = x : compress ys

-- | Problem 8 v2
compress' :: (Eq a) => [a] -> [a] -- non mio
compress' = map head . L.group

-- | Problem 9
pack :: (Eq a) => [a] -> [[a]] -- non mio
pack [] = []
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

-- | Problem 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = [(length x, head x) | x <- L.group xs]

nestedList :: NestedList Int
nestedList = List [Elem 12, List [Elem 1, Elem 3, List [Elem 0]], Elem 4]

list :: [Int]
list = [2,2,123,123,123,456,123,2,1,1,0,0]

main :: IO()
main = print $ compress' list