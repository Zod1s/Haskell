import Control.Monad
import Control.Monad.Trans(MonadTrans(..))
import Control.Applicative.Alternative

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
    -- return :: a -> MaybeT m a -- a -> m a
    return = MaybeT . return . Just
    -- (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b -- m a -> (a -> m b) -> m b
    x >>= f = MaybeT $ do mval <- runMaybeT x
                          case mval of
                              Nothing -> return Nothing
                              Just val -> runMaybeT $ f val

instance Monad m => Applicative (MaybeT m) where
    pure = return
    (<*>) = ap

instance Monad m => Functor (MaybeT m) where
    fmap = liftM

instance Monad m => Alternative (MaybeT m) where
    empty = MaybeT $ return Nothing
    x <|> y = MaybeT $ do maybe_value <- runMaybeT x
                          case maybe_value of
                               Nothing -> runMaybeT y
                               Just _ -> return maybe_value

instance Monad m => MonadPlus (MaybeT m) where 
    mzero = empty
    mplus = (<|>)

instance MonadTrans MaybeT where
    lift = MaybeT . (liftM Just)