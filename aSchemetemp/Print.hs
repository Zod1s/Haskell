{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Scheme.Print where

import Control.Monad (liftM)
import Scheme.Env
  ( Env,
    SchemeVal (Atom, List, String),
    bindVars,
    liftThrows,
    runIOThrows,
  )
import Scheme.Eval (eval, primitiveBindings)
import Scheme.Read (readExpr)
import System.IO (hFlush, hPutStrLn, stderr, stdout)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

until_ :: (Monad m) => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred' prompt action =
  do
    result <- prompt
    if pred' result
      then return ()
      else action result >> until_ pred' prompt action

runOne :: [String] -> IO ()
runOne args =
  do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    runIOThrows (show <$> eval env (List [Atom "load", String (head args)])) >>= hPutStrLn stderr

runRepl :: IO ()
runRepl =
  do
    env <- primitiveBindings -- creates the REPL environment
    evalString env "(load \"stdlib.scm\")"
    -- loads the standard library written in Scheme before starting the REPL session
    until_ (\x -> x == "quit" || x == "exit") (readPrompt "λ>> ") . evalAndPrint $ env
