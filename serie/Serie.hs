import Control.Monad(mapM_)

main :: IO()
main = mapM_ print (paramPartialSum 100 (-1.01) succession)

type Succession = Int -> Double
type ParamSuccession = Double -> Succession

succession :: ParamSuccession
succession a n = (2 * a - 1) ^^ n / (fromIntegral (2 * n + 3) ^ 2)

paramSeries :: Int -> Double -> ParamSuccession -> [Double]
paramSeries l a s = [sa n | n <- [1..l]]
  where sa = s a

paramPartialSum :: Int -> Double -> ParamSuccession -> [Double]
paramPartialSum l a s = scanl1 (+) $ paramSeries l a s
