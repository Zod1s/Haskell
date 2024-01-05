-- \| pure f <*> x = pure (flip ($)) <*> x <*> pure f = pure ($ x) <*> pure f

import Control.Applicative

{-
newtype ZipList a = ZipList {getZipList :: [a]}

instance Applicative ZipList where
    pure :: a -> ZipList a
    pure x = ZipList (repeat x)

    (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
    (ZipList gs) <*> (ZipList xs) = ZipList (zipWith ($) gs xs)
-}

{-
instance Applicative Maybe where
    pure = Just
    (Just f) <*> x = fmap f x
    Nothing <*> _ = Nothing
-}

sequenceAL :: (Applicative f) => [f a] -> f [a]
sequenceAL [] = pure []
sequenceAL (x : xs) = (:) <$> x <*> sequenceAL xs