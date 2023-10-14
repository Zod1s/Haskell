import           Control.Monad      (foldM)
import           System.Environment (getArgs)

solveRPN :: String -> Maybe Double
solveRPN st = do
  [result] <- foldM foldingFunction [] (words st)
  return result

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:ys) "*" = return ((x * y) : ys)
foldingFunction (x:y:ys) "+" = return ((x + y) : ys)
foldingFunction (x:y:ys) "-" = return ((y - x) : ys)
foldingFunction (x:y:ys) "/"
  | x /= 0 = return ((y / x) : ys)
  | otherwise = Nothing
foldingFunction (x:y:ys) "^" = return ((y ** x) : ys)
foldingFunction (x:xs) "ln"
  | x > 0 = return (log x : xs)
  | otherwise = Nothing
foldingFunction (x:xs) "log"
  | x > 0 = return (logBase 10 x : xs)
  | otherwise = Nothing
foldingFunction (x:y:xs) "logab"
  | x > 0 && y > 0 && y /= 1 = return (logBase y x : xs)
  | otherwise = Nothing
foldingFunction xs "sum" = return [sum xs]
foldingFunction xs numberString = fmap (: xs) (readMaybe numberString)

readMaybe :: (Read a) => String -> Maybe a
readMaybe st =
  case reads st of
    [(x, "")] -> Just x
    _         -> Nothing

main :: IO ()
main = do
  [list] <- getArgs
  print $ solveRPN list
