{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}

import Control.Monad.Writer
import MonadHandle
import System.IO (IOMode(..))

instance MonadHandle FilePath WriterIO where
    openFile path mode = tell [Open path mode] >> return path
    hPutStr h str = tell [Put h str]
    hClose h = tell [Close h]
    hGetContents h = tell [GetContents h] >> return ""

newtype WriterIO a = W { runW :: Writer [Event] a }
    deriving (Functor, Applicative, Monad, MonadWriter [Event])

runWriterIO :: WriterIO a -> (a, [Event])
runWriterIO = runWriter . runW

data Event = Open FilePath IOMode
           | Put String String
           | Close String
           | GetContents String
           deriving (Show)

safeHello :: MonadHandle h m => FilePath -> m ()
safeHello path = do
    h <- openFile path WriteMode
    hPutStrLn h "hello world"
    hClose h