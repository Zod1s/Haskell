{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Supply(
    Supply
  , next
  , runSupply) where

import Control.Monad.State

newtype Supply s a = S (State [s] a) 
  deriving (Functor, Applicative, Monad)

unwrapS :: Supply s a -> State [s] a
unwrapS (S s) = s

runSupply (S m) xs = runState m xs

next = S $ do
  st <- get
  case st of
    [] -> return Nothing
    (x:xs) -> do
      put xs
      return (Just x)
