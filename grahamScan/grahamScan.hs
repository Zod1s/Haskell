import Data.Function (on)
import Data.List

data Point = Point
  { x :: Double,
    y :: Double
  }

data Turn
  = DRight
  | DLeft
  | DStraight
  deriving (Show, Eq)

instance Eq Point where
  a == b = x a == x b && y a == y b

instance Show Point where
  show a = "(" ++ show (x a) ++ ", " ++ show (y a) ++ ")"

instance Num Point where
  (Point x1 y1) + (Point x2 y2) = Point (x1 + x2) (y1 + y2)
  (Point x1 y1) - (Point x2 y2) = Point (x1 - x2) (y1 - y2)
  (Point x1 y1) * (Point x2 y2) = error "Not Implemented"
  abs (Point _ _) = error "Not Implemented"
  signum (Point _ _) = error "Not Implemented"
  fromInteger _ = error "Not Implemented"

makePoint :: (Double, Double) -> Point
makePoint (x, y) = Point x y

modulus :: Point -> Double
modulus p = sqrt (x p ^ 2 + y p ^ 2)

dist :: Point -> Point -> Double
dist a b = modulus (b - a)

crossZ :: Point -> Point -> Point -> Double
crossZ a b c = x vec1 * y vec2 - y vec1 * x vec2
  where
    vec1 = b - a
    vec2 = c - a

angleTo :: Point -> Point -> Double
angleTo p1 p2
  | angle >= 0 = angle
  | angle < 0 = 2 * pi + angle
  where
    vec = p2 - p1
    angle = atan2 (y vec) (x vec)

start :: [Point] -> Point
start ps = minimumBy (compare `on` y) (sortBy (compare `on` x) ps)

sortRelativeTo :: Point -> [Point] -> [Point]
sortRelativeTo p ps = p : [maximumBy maxCond x | x <- groupBy groupCond $ sortBy compCond (delete p (nub ps))]
  where
    compCond = compare `on` angleTo p
    groupCond = (==) `on` angleTo p
    maxCond = compare `on` dist p

turnType :: Point -> Point -> Point -> Turn
turnType a b c
  | crossZ a b c > 0 = DLeft
  | crossZ a b c < 0 = DRight
  | otherwise = DStraight

convexHull :: [Point] -> Either String [Point]
convexHull list =
  if length list < 3
    then Left "Error, too few points"
    else hull sortedList []
  where
    sortedList = sortRelativeTo (start list) list
    hull points@(p : ps) stack
      | length stack > 1 && turnType (nextToTop stack) (top stack) p == DRight = hull points (tail stack)
      | null ps = Right (reverse (p : stack))
      | otherwise = hull ps (p : stack)
    top = head
    nextToTop = head . tail

main :: IO ()
main = do
  case convexHull pointList of
    Left err -> putStrLn err
    Right list -> print list

list :: [(Double, Double)]
list = [(2, 4), (3, 5), (4, 5), (7, 6), (7, 7), (-2, 3), (3, 1), (9, 2), (-1, 9)]

pointList :: [Point]
pointList = map makePoint list