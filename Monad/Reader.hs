import Data.Char as Char

newtype Reader e a = Reader {runReader :: e -> a}

ask :: Reader e e
ask = Reader id -- (\e -> e)
asks :: (e -> a) -> Reader e a
asks = Reader -- (\e -> fn e)
local :: (e -> e') -> Reader e' a -> Reader e a
local transform (Reader f) = Reader (f . transform)

instance Functor (Reader e) where
    -- fmap :: (a -> b) -> Reader e a -> Reader e b
    fmap f (Reader fn) = Reader (\e -> f . fn $ e)

instance Applicative (Reader e) where
    -- pure :: a -> Reader e a
    pure a = Reader (\_ -> a)
    -- (<*>) :: (<*>) :: Reader e (a -> b) -> Reader e a -> Reader e b
    (Reader fn) <*> (Reader fa) = Reader (\e -> let a = fa e in fn e a)

instance Monad (Reader e) where
    -- return :: a -> Reader e a
    return a = Reader (\_ -> a)
    -- (>>=) :: Reader e a -> (a -> Reader e b) -> Reader e b
    (Reader g) >>= f = Reader (\e -> runReader (f . g $ e) e)

-- # ----------------------------------------------- # --

-- Our config data. We're very particular about certain
-- letters of the alphabet, see.

data ABConfig = ABConfig {
    don'tUseLetterE :: Bool, 
    don'tUseLetterL :: Bool}

type Config a = Reader ABConfig a

-- Uppercase the string, obeying current tests.
toUpperStr :: String -> Config String
toUpperStr str = do 
    cfg <- ask
    let filters = [if don'tUseLetterE cfg then (/= 'E') else const True, if don'tUseLetterL cfg then (/= 'L') else const True] :: [Char -> Bool]
    let passesFilters c = all (\f -> f c) filters-- :: Char -> Bool
    pure (filter passesFilters (fmap Char.toUpper str))

fullName :: String -> String -> String -> Config String
fullName name nick sur = do
    newName <- toUpperStr name
    newSur <- toUpperStr sur
    newNick <- toUpperStr nick
    pure (newName ++ " \"" ++ newNick ++ " \"" ++ newSur)
