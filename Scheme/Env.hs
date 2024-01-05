{-# LANGUAGE FlexibleContexts #-}

module Env where

import Control.Monad.Except
  ( ExceptT,
    MonadError (catchError, throwError),
    MonadIO (liftIO),
    liftM,
    runExceptT,
  )
import Data.Array (Array)
import Data.Complex (Complex (..), imagPart, realPart)
-- Rifare con Megaparsec
import Data.Functor ((<&>))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Ratio (denominator, numerator)
import Numeric (showFFloat)
import System.IO (Handle)
import Text.ParserCombinators.Parsec (ParseError)

data SchemeVal
  = Atom String -- Types in Scheme
  | List [SchemeVal]
  | DottedList [SchemeVal] SchemeVal
  | Vector (Array Int SchemeVal)
  | Number Integer
  | Float Double
  | Ratio Rational
  | Complex (Complex Double)
  | String String
  | Character Char
  | Bool Bool
  | PrimitiveFunc ([SchemeVal] -> ThrowsError SchemeVal)
  | Func
      { isMacro :: Bool,
        params :: [String],
        vararg :: Maybe String,
        body :: [SchemeVal],
        closure :: Env
      }
  | IOFunc ([SchemeVal] -> IOThrowsError SchemeVal)
  | Port Handle

instance Eq SchemeVal where
  (Atom a) == (Atom b) = a == b
  (List a) == (List b) = a == b
  (DottedList a b) == (DottedList c d) = a == c && b == d
  (Number a) == (Number b) = a == b
  (Float a) == (Float b) = a == b
  (Complex a) == (Complex b) = a == b
  (Ratio a) == (Ratio b) = a == b
  (Bool a) == (Bool b) = a == b
  (Character a) == (Character b) = a == b
  (Vector a) == (Vector b) = a == b
  (Bool a) == _ = a
  (Number a) == (Float b) = fromInteger a == b
  (Float a) == (Number b) = a == fromInteger b
  (Number a) == (Complex b) = fromInteger a :+ 0 == b
  (Complex a) == (Number b) = a == fromInteger b :+ 0
  (Number a) == (Ratio b) = fromInteger a == b
  (Ratio a) == (Number b) = a == fromInteger b
  (Complex a) == (Float b) = a == b :+ 0
  (Float a) == (Complex b) = a :+ 0 == b
  (Ratio a) == (Float b) = a == realToFrac b
  (Float a) == (Ratio b) = realToFrac a == b
  (Complex a) == (Ratio b) = a == fromRational b :+ 0
  (Ratio a) == (Complex b) = fromRational a :+ 0 == b
  _ == _ = False -- modificare Eval.hs per tenerne conto

instance Ord SchemeVal where
  compare (Number x) (Number y) = compare x y
  compare (Float x) (Float y) = compare x y
  compare (Ratio x) (Ratio y) = compare x y
  compare (Number x) (Ratio y) = compare (fromInteger x) y
  compare (Number x) (Float y) = compare (fromInteger x) y
  compare (Float x) (Number y) = compare x (fromInteger y)
  compare (Ratio x) (Number y) = compare x (fromInteger y)
  compare (Float x) (Ratio y) = compare (realToFrac x) y
  compare (Ratio x) (Float y) = compare x (realToFrac y)
  compare (Complex _) _ =
    error
      "Complex numbers do not satisfy the axiom of trichotomy"
  compare _ (Complex _) =
    error
      "Complex numbers do not satisfy the axiom of trichotomy"
  compare (String x) (String y) = compare x y
  compare (Character x) (Character y) = compare x y
  compare (Bool x) (Bool y) = compare x y
  compare _ _ = error "comparing values of different type"

instance Show SchemeVal where
  show (Atom name) = name
  show (List contents) = "(" ++ unwordsList contents ++ ")"
  show (DottedList head' tail') = "(" ++ unwordsList head' ++ " . " ++ show tail' ++ ")"
  show (Number contents) = show contents
  show (String contents) = "\"" ++ contents ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Float f) = showFullPrecision f
  show (Ratio r) = (show . numerator) r ++ "/" ++ (show . denominator) r
  show (Complex c) =
    (showFullPrecision . realPart) c
      ++ (if imagPart c >= 0 then "+" else "")
      ++ (showFullPrecision . imagPart) c
      ++ "i"
  show (Character '\n') = ""
  show (Character contents) = "#\\" ++ [contents]
  show (Vector contents) = show contents
  show (PrimitiveFunc _) = "<primitive>"
  show (Func {params = args, vararg = varargs, body = _, closure = _}) =
    "(lambda("
      ++ unwords (map show args)
      ++ ( case varargs of
             Nothing -> ""
             Just arg -> " . " ++ arg
         )
      ++ ") ...)"
  show (Port _) = "<IO port>"
  show (IOFunc _) = "<IO primitive>"

showFullPrecision :: Double -> String
showFullPrecision x = showFFloat Nothing x ""

data SchemeError
  = NumArgs Integer [SchemeVal]
  | TypeMismatch String SchemeVal
  | Parser ParseError
  | BadSpecialForm String SchemeVal
  | NotFunction String String
  | UnboundVar String String
  | Default String
  | DivideByZero
  | ReservedKeyword String

instance Show SchemeError where
  show (NumArgs expected found) =
    "Expected "
      ++ show expected
      ++ " args; found values "
      ++ unwordsList found
  show (TypeMismatch expected found) =
    "Invalid type: expected "
      ++ expected
      ++ ", found "
      ++ show found
  show (Parser parseErr) = "Parse error at " ++ show parseErr
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func) = message ++ ": " ++ show func
  show (UnboundVar message varname) = message ++ ": " ++ varname
  show (Default message) = message
  show DivideByZero = "Divide by Zero Error"
  show (ReservedKeyword key) = "Reserved Keyword \"" ++ key ++ "\" used in definition"

type ThrowsError = Either SchemeError

type IOThrowsError = ExceptT SchemeError IO

type Env = IORef [(String, IORef SchemeVal)]

unwordsList :: [SchemeVal] -> String
unwordsList = unwords . map show

trapError :: IOThrowsError String -> IOThrowsError String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left _) = error "Cannot extract value from Error"

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) <&> extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef <&> isJust . lookup var

getVar :: Env -> String -> IOThrowsError SchemeVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Env -> String -> SchemeVal -> IOThrowsError SchemeVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Setting an unbound variable" var)
    (liftIO . (`writeIORef` value))
    (lookup var env)
  return value

defineVar :: Env -> String -> SchemeVal -> IOThrowsError SchemeVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

getVars :: Env -> IOThrowsError [(String, SchemeVal)]
getVars envRef = do
  env <- liftIO $ readIORef envRef
  let vars = map fst env
  vals <- traverse (getVar envRef) vars
  return $ zip vars vals

makeFunc :: Bool -> Maybe String -> Env -> [SchemeVal] -> [SchemeVal] -> IOThrowsError SchemeVal
makeFunc isMacro' varargs env' params' body' =
  return $
    Func
      isMacro'
      (map show params')
      varargs
      body'
      env'

makeNormalFunc :: Env -> [SchemeVal] -> [SchemeVal] -> IOThrowsError SchemeVal
makeNormalFunc = makeFunc False Nothing

makeVarArgs :: SchemeVal -> Env -> [SchemeVal] -> [SchemeVal] -> IOThrowsError SchemeVal
makeVarArgs = makeFunc False . Just . show

makeMacro :: Env -> [SchemeVal] -> [SchemeVal] -> IOThrowsError SchemeVal
makeMacro = makeFunc True Nothing

bindVars :: Env -> [(String, SchemeVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings' env = fmap (++ env) (mapM addBinding bindings')
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)
