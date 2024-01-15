{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Monad.Writer
import IO.MonadHandle
import System.IO (IOMode (..))

instance MonadHandle FilePath WriterIO where
  openFile :: FilePath -> IOMode -> WriterIO FilePath
  openFile path mode = tell [Open path mode] >> return path
  hPutStr :: FilePath -> String -> WriterIO ()
  hPutStr h str = tell [Put h str]
  hClose :: FilePath -> WriterIO ()
  hClose h = tell [Close h]
  hGetContents :: FilePath -> WriterIO String
  hGetContents h = tell [GetContents h] >> return ""

newtype WriterIO a = W {runW :: Writer [Event] a}
  deriving (Functor, Applicative, Monad, MonadWriter [Event])

runWriterIO :: WriterIO a -> (a, [Event])
runWriterIO = runWriter . runW

data Event
  = Open FilePath IOMode
  | Put String String
  | Close String
  | GetContents String
  deriving (Show)

safeHello :: (MonadHandle h m) => FilePath -> m ()
safeHello path = do
  h <- openFile path WriteMode
  hPutStrLn h "hello world"
  hClose h