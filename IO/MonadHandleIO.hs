{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Monad.Trans (MonadIO (..), MonadTrans (..))
import IO.MonadHandle
import System.Directory (removeFile)
import System.IO (IOMode (..))
import System.IO qualified

instance MonadHandle System.IO.Handle IO where
  openFile = System.IO.openFile
  hPutStr = System.IO.hPutStr
  hClose = System.IO.hClose
  hGetContents = System.IO.hGetContents
  hPutStrLn = System.IO.hPutStrLn

safeHello :: (MonadHandle h m) => FilePath -> m ()
safeHello path = do
  h <- openFile path WriteMode
  hPutStrLn h "hello world"
  hClose h