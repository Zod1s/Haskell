import qualified Data.List as L

data ListItem a = Single a 
                | Multiple Int a
    deriving (Show)

-- | Problem 11
encode :: (Eq a) => [a] -> [ListItem a]
encode xs = [if length x > 1 then Multiple (length x) (head x) else Single (head x) | x <- L.group xs]

-- | Problem 12
decode :: (Eq a) => [ListItem a] -> [a]
decode [] = []
decode xs = case (head xs) of 
    (Single x) -> x : decode (tail xs)
    (Multiple times x) -> replicate times x ++ decode (tail xs)

-- | Problem 13
encode' :: (Eq a) => [a] -> [(Int,a)] -- non mio
encode' = foldr helper []
    where helper x [] = [(1,x)]
          helper x (y@(a,b):ys)
            | x == b    = (1+a,x):ys
            | otherwise = (1,x):y:ys

encodeDirect :: (Eq a) => [a] -> [ListItem a] -- non mio
encodeDirect = map encodeHelper . encode'
    where encodeHelper (1,x) = Single x
          encodeHelper (n,x) = Multiple n x

-- | Problem 14
duplicate :: [a] -> [a]
duplicate [x] = x:x:[]
duplicate (x:xs) = x:x:(duplicate xs)

-- | Problem 15
nReplicate :: Int -> [a] -> [a]
nReplicate times [x] = replicate times x
nReplicate times (x:xs) = replicate times x ++ nReplicate times xs

-- | Problem 16
dropEvery :: Int -> [a] -> [a]
dropEvery _ [] = []
dropEvery n xs = take (n-1) xs ++ dropEvery n (drop (n) xs)

-- | Problem 17
split :: Int -> [a] -> ([a], [a])
split index xs = (take index xs, drop index xs)

-- | Problem 18
slice :: Int -> Int -> [a] -> [a]
slice l k xs = take (k - l + 1) $ drop (l - 1) xs

-- | Problem 19
rotate :: Int -> [a] -> [a]
rotate i xs
    | i >= 0 = drop i xs ++ take i xs
    | otherwise = drop j xs ++ take j xs
    where j = i + length xs

-- | Problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt _ [] = error "Lista Vuota"
removeAt index xs = (xs !! index, take index xs ++ drop (index + 1) xs)

list :: [Int]
list = [2,2,123,123,123,456,123,2,1,1,0,0]

toDecode :: [ListItem Int]
toDecode = [Multiple 2 2,Multiple 3 123,Single 456,Single 123,Single 2,Multiple 2 1,Multiple 2 0]

main :: IO()
main = print $ removeAt 0 ['a','b','c','d','e','f','g','h']