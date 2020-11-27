{-
instance Monad [] where
    return :: a -> [a]
    return x = [x]
    (>>=) :: [a] -> (a -> [b]) -> [b]
    xs >>= g = concat (map g xs)
-}
import Control.Monad

data Free f a = Var a
              | Node (f (Free f a))

instance (Functor f) => Functor (Free f) where
    fmap g (Var x) = Var (g x)
    fmap g (Node x) = Node (fmap (fmap g) x)

instance (Applicative f) => Applicative (Free f) where
    pure = Var
    (Var f) <*> x = fmap f x

instance (Applicative f) => Monad (Free f) where
    return = Var
    (Var a) >>= f = f a
    (Node x) >>= f = Node (fmap (>>= f) x)

(>>>=) :: (Monad m) => m a -> (a -> m b) -> m b
a >>>= b = join (fmap b a)

fmap' :: (Monad f) => (a -> b) -> f a -> f b
fmap' g = ((return . g) =<<)

join' :: (Monad m) => m (m a) -> m a
join' = (id =<<)