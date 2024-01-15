{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Monad.Trans (MonadIO (..), MonadTrans (..))
import IO.MonadHandle
import System.Directory (removeFile)
import System.IO (IOMode (..))
import System.IO qualified

instance MonadHandle System.IO.Handle IO where
  openFile :: FilePath -> IOMode -> IO System.IO.Handle
  openFile = System.IO.openFile
  hPutStr :: System.IO.Handle -> String -> IO ()
  hPutStr = System.IO.hPutStr
  hClose :: System.IO.Handle -> IO ()
  hClose = System.IO.hClose
  hGetContents :: System.IO.Handle -> IO String
  hGetContents = System.IO.hGetContents
  hPutStrLn :: System.IO.Handle -> String -> IO ()
  hPutStrLn = System.IO.hPutStrLn

safeHello :: (MonadHandle h m) => FilePath -> m ()
safeHello path = do
  h <- openFile path WriteMode
  hPutStrLn h "hello world"
  hClose h