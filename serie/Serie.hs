import Control.Monad(mapM_)

main :: IO()
main = mapM_ print (partialSum 1000 succession)

type Succession = Int -> Double
type ParamSuccession = Double -> Succession

succession :: Succession
succession n = cos(fromIntegral n) * sin(1.0 / (fromIntegral n))

series :: Int -> Succession -> [Double]
series l s = [s n | n <- [1..l]]

partialSum :: Int -> Succession -> [Double]
partialSum l s = scanl1 (+) $ series l s

paramSuccession :: ParamSuccession
paramSuccession a n = (2 * a - 1) ^^ n / (fromIntegral (2 * n + 3) ^ 2)

paramSeries :: Int -> Double -> ParamSuccession -> [Double]
paramSeries l a s = [sa n | n <- [1..l]]
  where sa = s a

paramPartialSum :: Int -> Double -> ParamSuccession -> [Double]
paramPartialSum l a s = scanl1 (+) $ paramSeries l a s
