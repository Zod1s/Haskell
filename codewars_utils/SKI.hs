{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}


data SKI :: * -> * where
    Ap :: SKI (a -> b) -> SKI a -> SKI b
    S :: SKI ((a -> b -> c) -> (a -> b) -> a -> c)
    K :: SKI (a -> b -> a)
    I :: SKI (a -> a)