{-# LANGUAGE BangPatterns #-}

import Data.List
import Data.Char
import Data.Function (on)
import Control.Monad(mapM_)

-- | Problema 1
sumOf :: Int
sumOf = sum [x | x <- [1..999], rem x 3 == 0 || rem x 5 == 0]

-- | Problema 2
fib :: [Integer]
fib = 1 : 1 : zipWith (+) fib (tail fib)

fiboSum :: Integer
fiboSum = sum $ filter even $ takeWhile (<4000000) fib

-- | Problema 3
isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

sieve :: Integer -> [Integer]
sieve n = filter isPrime [2..n]
  where isPrime a = null [x | x <- [2..isqrt a], rem a x == 0]

isFactor :: Integer -> Integer -> Bool
isFactor n1 n2 = rem n1 n2 == 0

factorize :: (Integral a) => a -> [a] -- non mio, più efficiente
factorize n = factorize' n 2 where
  factorize' 1 _ = []
  factorize' n factor
    | n `mod` factor == 0 = factor : factorize' (n `div` factor) factor
    | otherwise           = factorize' n (factor + 1)

largest :: Integer
largest = last $ factorize 600851475143

-- | Problema 4
reverseInt :: Integer -> Integer
reverseInt = read . reverse . show

isPalindrome :: Integer -> Bool
isPalindrome n = n == reverseInt n

biggest :: Integer
biggest = maximum $ filter isPalindrome $ zipWith (*) [x | x <- [999,998..100]] [y | y <- [999,998..100]]

biggest2 :: Integer
biggest2 = maximum [x * y | x <- [999,998..100], y <- [999,998..100], isPalindrome $ x * y]

-- | Problema 5
isDivisibleBy1_20 :: Integer -> Bool
isDivisibleBy1_20 n = divisible n [1..20]
  where divisible a (x:xs) = isFactor a x && divisible a xs
        divisible a [] = True

maxDivisible :: Integer
maxDivisible = head [x | x <- [1..], isDivisibleBy1_20 x]

-- | Problema 6
squareSum :: Integer
squareSum = sum [x^2 | x <- [1..100]]

sumSquare :: Integer
sumSquare = (^) (sum [x | x <- [1..100]]) 2

-- | Problema 7
takeNthPrime :: Integer -> Integer
takeNthPrime n = filterPrime [2..] n
  where filterPrime :: [Integer] -> Integer -> Integer
        filterPrime (x:xs) 1 = x
        filterPrime (x:xs) n = filterPrime (filter (\y -> rem y x /= 0) xs) (n - 1)

-- | Problema 8
bigNumber :: [Char]
bigNumber = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"

createGroups :: [[Int]]
createGroups = map (take groupsDim) $ drop groupsDim $ reverse $ tails digits
  where groupsDim = 13
        digits = map digitToInt bigNumber

maxProd :: Int
maxProd = maximum [p | a <- createGroups, let p = foldl1' (*) a] -- con foldr1 occupa più spazio

-- | Problema 9
pythTrip :: Integer
pythTrip = head [a * b * c |
                  a <- [1..500], b <- [1..500], c <- [1..500],
                  a^2 + b^2 == c^2, a + b + c == 1000, a<=b, b<=c]

-- | Problema 10
sumPrimes :: Integer
sumPrimes = sum $ sieve 2000000

-- | Problema 11 -- non risolto

-- | Problema 12
findDivisors :: Integer -> [Integer]
findDivisors n = (filter (isFactor n) [1..half]) ++ [n]
  where half = div n 2

triangles :: [Integer]
triangles = scanl1 (+) [1..]

firstOver500 :: Integer
firstOver500 = head (filter ((>500) . (numDivisors)) triangles)
  where numDivisors n = length (findDivisors n)

-- | Problema 13
numberList :: [Integer]
numberList = [
  37107287533902102798797998220837590246510135740250,
  46376937677490009712648124896970078050417018260538,
  74324986199524741059474233309513058123726617309629,
  91942213363574161572522430563301811072406154908250,
  23067588207539346171171980310421047513778063246676,
  89261670696623633820136378418383684178734361726757,
  28112879812849979408065481931592621691275889832738,
  44274228917432520321923589422876796487670272189318,
  47451445736001306439091167216856844588711603153276,
  70386486105843025439939619828917593665686757934951,
  62176457141856560629502157223196586755079324193331,
  64906352462741904929101432445813822663347944758178,
  92575867718337217661963751590579239728245598838407,
  58203565325359399008402633568948830189458628227828,
  80181199384826282014278194139940567587151170094390,
  35398664372827112653829987240784473053190104293586,
  86515506006295864861532075273371959191420517255829,
  71693888707715466499115593487603532921714970056938,
  54370070576826684624621495650076471787294438377604,
  53282654108756828443191190634694037855217779295145,
  36123272525000296071075082563815656710885258350721,
  45876576172410976447339110607218265236877223636045,
  17423706905851860660448207621209813287860733969412,
  81142660418086830619328460811191061556940512689692,
  51934325451728388641918047049293215058642563049483,
  62467221648435076201727918039944693004732956340691,
  15732444386908125794514089057706229429197107928209,
  55037687525678773091862540744969844508330393682126,
  18336384825330154686196124348767681297534375946515,
  80386287592878490201521685554828717201219257766954,
  78182833757993103614740356856449095527097864797581,
  16726320100436897842553539920931837441497806860984,
  48403098129077791799088218795327364475675590848030,
  87086987551392711854517078544161852424320693150332,
  59959406895756536782107074926966537676326235447210,
  69793950679652694742597709739166693763042633987085,
  41052684708299085211399427365734116182760315001271,
  65378607361501080857009149939512557028198746004375,
  35829035317434717326932123578154982629742552737307,
  94953759765105305946966067683156574377167401875275,
  88902802571733229619176668713819931811048770190271,
  25267680276078003013678680992525463401061632866526,
  36270218540497705585629946580636237993140746255962,
  24074486908231174977792365466257246923322810917141,
  91430288197103288597806669760892938638285025333403,
  34413065578016127815921815005561868836468420090470,
  23053081172816430487623791969842487255036638784583,
  11487696932154902810424020138335124462181441773470,
  63783299490636259666498587618221225225512486764533,
  67720186971698544312419572409913959008952310058822,
  95548255300263520781532296796249481641953868218774,
  76085327132285723110424803456124867697064507995236,
  37774242535411291684276865538926205024910326572967,
  23701913275725675285653248258265463092207058596522,
  29798860272258331913126375147341994889534765745501,
  18495701454879288984856827726077713721403798879715,
  38298203783031473527721580348144513491373226651381,
  34829543829199918180278916522431027392251122869539,
  40957953066405232632538044100059654939159879593635,
  29746152185502371307642255121183693803580388584903,
  41698116222072977186158236678424689157993532961922,
  62467957194401269043877107275048102390895523597457,
  23189706772547915061505504953922979530901129967519,
  86188088225875314529584099251203829009407770775672,
  11306739708304724483816533873502340845647058077308,
  82959174767140363198008187129011875491310547126581,
  97623331044818386269515456334926366572897563400500,
  42846280183517070527831839425882145521227251250327,
  55121603546981200581762165212827652751691296897789,
  32238195734329339946437501907836945765883352399886,
  75506164965184775180738168837861091527357929701337,
  62177842752192623401942399639168044983993173312731,
  32924185707147349566916674687634660915035914677504,
  99518671430235219628894890102423325116913619626622,
  73267460800591547471830798392868535206946944540724,
  76841822524674417161514036427982273348055556214818,
  97142617910342598647204516893989422179826088076852,
  87783646182799346313767754307809363333018982642090,
  10848802521674670883215120185883543223812876952786,
  71329612474782464538636993009049310363619763878039,
  62184073572399794223406235393808339651327408011116,
  66627891981488087797941876876144230030984490851411,
  60661826293682836764744779239180335110989069790714,
  85786944089552990653640447425576083659976645795096,
  66024396409905389607120198219976047599490197230297,
  64913982680032973156037120041377903785566085089252,
  16730939319872750275468906903707539413042652315011,
  94809377245048795150954100921645863754710598436791,
  78639167021187492431995700641917969777599028300699,
  15368713711936614952811305876380278410754449733078,
  40789923115535562561142322423255033685442488917353,
  44889911501440648020369068063960672322193204149535,
  41503128880339536053299340368006977710650566631954,
  81234880673210146739058568557934581403627822703280,
  82616570773948327592232845941706525094512325230608,
  22918802058777319719839450180888072429661980811197,
  77158542502016545090413245809786882778948721859617,
  72107838435069186155435662884062257473692284509516,
  20849603980134001723930671666823555245252804609722,
  53503534226472524250874054075591789781264330331690]

takeFirstTenDigits :: [Char]
takeFirstTenDigits = take 10 $ show $ sum numberList

-- | Problema 14
collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n
  | rem n 4 == 0 = n : collatz (div n 4)
  | rem n 4 == 1 = n : collatz (1 + 3 * g n)
  | rem n 4 == 2 = n : collatz (div n 2)
  | rem n 4 == 3 = n : collatz (div (3 * n + 1) 2)
  where g n
          | rem n 4 == 1 = g (div (n - 1) 4)
          | otherwise = n
  
problem_14 :: [Integer]
problem_14 = foldl1' (\acc x -> if (length x) > (length acc) then x else acc) (map collatz [1..1000000])

-- | Problema 15
factorial :: Integer -> Integer
factorial n
  | n < 3 = n
  | otherwise = n * factorial (n - 1)

binomialCoeff :: Integer -> Integer -> Integer
binomialCoeff n k = div (factorial n) (factorial k * factorial(n - k))

problem_15 :: Integer
problem_15 = binomialCoeff 40 20

-- | Problema 16
numberToList :: Integer -> [Int]
numberToList n = map digitToInt (show n)

problem_16 :: Integer
problem_16 = sum $ map fromIntegral $ numberToList (2^1000)

-- | Problema 17
toTwenty :: [String]
toTwenty = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"
           , "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

pastTwenty :: [String]
pastTwenty = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

toLetters :: Int -> String
toLetters n
  | 0 <= n && n < 20 = toTwenty !! n
  | 20 <= n && n < 100 = pastTwenty !! (div n 10) ++ (if rem n 10 /= 0 then (toTwenty !! (rem n 10)) else "")
  | 100 <= n && n < 1000 = toTwenty !! (div n 100) ++ "hundred" ++ (if rem n 100 /= 0
                                                                    then "and" ++ toLetters (rem n 100)
                                                                    else "")
  | otherwise = "onethousand"

problem_17 :: Int
problem_17 = sum $ map length [toLetters x | x <- [1..1000]]

-- | Problema 18
triangle18 :: [[Int]]
triangle18 = [
  [75],
  [95, 64],
  [17, 47, 82],
  [18, 35, 87, 10],
  [20,  4, 82, 47, 65],
  [19,  1, 23, 75,  3, 34],
  [88, 02, 77, 73,  7, 63, 67],
  [99, 65,  4, 28,  6, 16, 70, 92],
  [41, 41, 26, 56, 83, 40, 80, 70, 33],
  [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
  [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
  [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
  [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
  [63, 66,  4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
  [04, 62, 98, 27, 23,  9, 70, 98, 73, 93, 38, 53, 60,  4, 23]]

problem_18 :: Int
problem_18 = head $ foldr1 g triangle18
  where f x y z = x + max y z
        g xs ys = zipWith3 f xs ys $ tail ys

-- | Problema 19
zeller :: Int -> Int -> Int -> Bool
zeller y m d = rem (d + div (13*(month+1)) 5 + year + div year 4 - div year 100 + div year 400) 7 - 1 == 0
  where year
          | m < 3 = y - 1
          | otherwise = y
        month
          | m < 3 = m + 12
          | otherwise = m

problem_19 :: Int
problem_19 = sum [1 | y <- [1901..2000], m <- [1..12], zeller y m 1]

-- | Problema 20
problem_20 :: Int
problem_20 = sum . (map digitToInt) . show . factorial $ 100

-- | Problema 21
findProperDivisors :: Integer -> [Integer]
findProperDivisors n = (filter (isFactor n) [1..half])
  where half = div n 2

sumOfDivisors :: Integer -> Integer
sumOfDivisors = sum . findProperDivisors

problem_21 :: Integer
problem_21 = sum [a | a <- [1..9999], let b = sumOfDivisors a, a == sumOfDivisors b, a /= b]

-- | Problema 22 -- non risolto


-- | Problema 23 -- non risolto
notSumOfAbundant :: Integer -> Bool
notSumOfAbundant n = True

problem_23 :: Integer
problem_23 = fromIntegral $ sum [n | n <- [24..28123], notSumOfAbundant n]

-- | Problema 24

firstTenDigits :: [Char]
firstTenDigits = "0123456789"

allPermutations :: [String]
allPermutations = permutations firstTenDigits

millionthTerm :: String
millionthTerm = (sort allPermutations) !! 999999

-- | Problema 25
problem_25 :: Int
problem_25 = (+) 1 $ length . takeWhile (<10^999) $ fib

-- | Problema 26
problem_26 :: Int
problem_26 = maximum $ map decLength [x | x <- [1..999], x `rem` 2 /= 0, x `rem` 5 /= 0]

decLength :: Int -> Int
decLength i = length $ show (isMult 9)
  where isMult n = if n `rem` i == 0
                   then n
                   else isMult (n * 10 + 9)

-- | Problema 28
grid :: [[Integer]]
grid = [ [43, 44, 45, 46, 47, 48, 49]
       , [42, 21, 22, 23, 24, 25, 26]
       , [41, 20,  7,  8,  9, 10, 27]
       , [40, 19,  6,  1,  2, 11, 28]
       , [39, 18,  5,  4,  3, 12, 29]
       , [38, 17, 16, 15, 14, 13, 30]
       , [37, 36, 35, 34, 33, 32, 31]]

printGrid :: Show a => [[a]] -> IO()
printGrid grid = mapM_ printLine grid
  where maxLength = length $ maximumBy (compare `on` length)
          [maximumBy (compare `on` length) x | x <- (map (map show) grid)]
        printLine line = putStr "|" >> mapM_ (putStr . (pad maxLength) . show) line >> putStrLn "|"
        pad !l s = if l == (length s)
                   then ' ' : s
                   else ' ' : (pad (l - 1) s)

problem_28 :: Integer
problem_28 = 1 + sum [4 * x^2 - (x - 1) - 2 * (x - 1) - 3 * (x - 1) | x <- [3,5..1001]]

-- | Problema 29
problem_29 :: Int
problem_29 = length . nub $ [a^b | a <- [2..100], b <- [2..100]]

-- | Problema 40

helpList :: String
helpList = concat (map show [1..])

problem_40 :: Int
problem_40 = d100 * d1000 * d10000 * d100000 * d1000000
  where d100, d1000, d10000, d100000, d1000000 :: Int
        d100 = digitToInt $ head $ drop 99 helpList
        d1000 = digitToInt $ head $ drop 999 helpList
        d10000 = digitToInt $ head $ drop 9999 helpList
        d100000 = digitToInt $ head $ drop 99999 helpList
        d1000000 = digitToInt $ head $ drop 999999 helpList

-- | main
main :: IO()
main = print problem_40
