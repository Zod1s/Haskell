-- | fmap :: (a -> b) -> f a -> f b where f is a functor

-- instance Functor (Either e) where -- fmap :: (a -> b) -> Either e a -> Either e b
--     fmap _ (Left e) = Left e
--     fmap g (Right x) = Right (g x)

-- instance Functor ((->) e) where -- fmap :: (a -> b) -> ((->) e a) -> ((->) e b) = (a -> b) -> (e -> a) -> (e -> b)
--     fmap :: (a -> b) -> (e -> a) -> (e -> b)
--     fmap f g x = f . g $ x

-- instance Functor ((,) e) where -- fmap :: (a -> b) -> (,) e a -> (,) e b = (a -> b) -> (e, a) -> (e, b)
--     fmap g (e, x) = (e, g x)

data Pair a = Pair a a
  deriving (Show)

instance Functor Pair where -- fmap :: (a -> b) -> Pair a a -> Pair b b
  fmap g (Pair x y) = Pair (g x) (g y)

data ITree a
  = Leaf (Int -> a)
  | Node [ITree a]

instance Functor ITree where
  fmap f (Leaf g) = Leaf (f . g)
  fmap f (Node l) = Node (map (fmap f) l)