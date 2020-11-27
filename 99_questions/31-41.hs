import Data.Function (on)
import qualified Data.List as L

-- Problema 31
isPrime :: Int -> Bool
isPrime n = null [x | x <- [2..isqrt n], rem n x == 0]

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

-- Problema 32
gcd' :: Int -> Int -> Int
gcd' a b = if b /= 0
          then gcd' b (a `rem` b)
          else a

-- Problema 33
isCoprime :: Int -> Int -> Bool
isCoprime a b = if gcd' a b == 1
                then True
                else False

-- Problema 34
totient :: Int -> Int
totient n = length . filter (isCoprime n) $ [1..n-1]

-- Problema 35
primeFactors :: Int -> [Int]
primeFactors n = factors n (sieve n)
    where factors n iss@(i:is) = if n `rem` i == 0
                                 then (i) : factors (n `div` i) iss
                                 else factors n is
          factors _ [] = []

sieve :: Int-> [Int]
sieve n = filter isPrime [2..n]

-- Problema 36
primeFactors' :: Int -> [(Int, Int)]
primeFactors' n = [(head x, length x) | x <-L.group (primeFactors n)]

-- Problema 37
totient' :: Int -> Int
totient' n = product [(x - 1) * x^(m - 1) | (x,m) <- primeFactors' n]

-- Problema 39
sieveFT :: Int -> Int -> [Int]
sieveFT f t = filter isPrime [f..t]

-- Problema 40
goldbach :: Int -> (Int, Int)
goldbach n = head [(x,y) | x <- primes, y <- primes, x + y == n]
    where primes = sieve n

-- Problema 41
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList f l = map goldbach $ filter even [f..l]

main :: IO()
main = print $ goldbachList' 2 3000