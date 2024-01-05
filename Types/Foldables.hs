{-class Foldable t where
    fold    :: Monoid m => t m -> m
    foldMap :: Monoid m => (a -> m) -> t a -> m
    foldr   :: (a -> b -> b) -> b -> t a -> b
    foldr'  :: (a -> b -> b) -> b -> t a -> b
    foldl   :: (b -> a -> b) -> b -> t a -> b
    foldl'  :: (b -> a -> b) -> b -> t a -> b
    foldr1  :: (a -> a -> a) -> t a -> a
    foldl1  :: (a -> a -> a) -> t a -> a
    toList  :: t a -> [a]
    null    :: t a -> Bool
    length  :: t a -> Int
    elem    :: Eq a => a -> t a -> Bool
    maximum :: Ord a => t a -> a
    minimum :: Ord a => t a -> a
    sum     :: Num a => t a -> a
    product :: Num a => t a -> a

instance Foldable [] where
    foldMap :: Monoid m => (a -> m) -> [a] -> m
    foldMap g = mconcat . map g
-}

import Data.Foldable hiding (fold)

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Show)

instance Foldable Tree where
  -- foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l k r) = foldMap f l `mappend` f k `mappend` foldMap f r

fold :: (Monoid m, Foldable t) => t m -> m
fold = foldMap id