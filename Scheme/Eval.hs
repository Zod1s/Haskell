{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Eval where

import Control.Monad.Except
  ( MonadError (catchError, throwError),
    MonadIO (liftIO),
    liftM,
  )
import Data.Array (bounds, elems, listArray, (!))
import Data.Char
  ( isAlpha,
    isLower,
    isNumber,
    isSpace,
    isUpper,
    toLower,
    toUpper,
  )
import Data.Complex (Complex (..), imagPart, magnitude, mkPolar, realPart)
import Data.Fixed (mod')
import Data.Foldable (foldlM)
import Data.Functor ((<&>))
import Data.Maybe (isNothing)
import Data.Ratio (denominator, numerator)
import Env
  ( Env,
    IOThrowsError,
    SchemeError
      ( BadSpecialForm,
        Default,
        DivideByZero,
        NotFunction,
        NumArgs,
        ReservedKeyword,
        TypeMismatch
      ),
    SchemeVal
      ( Atom,
        Bool,
        Character,
        Complex,
        DottedList,
        Float,
        Func,
        IOFunc,
        List,
        Number,
        Port,
        PrimitiveFunc,
        Ratio,
        String,
        Vector,
        isMacro
      ),
    ThrowsError,
    bindVars,
    defineVar,
    getVar,
    liftThrows,
    makeMacro,
    makeNormalFunc,
    makeVarArgs,
    nullEnv,
    setVar,
  )
import Read (readExpr, readExprList)
import System.IO
  ( IOMode (ReadMode, WriteMode),
    hClose,
    hGetLine,
    hIsReadable,
    hIsWritable,
    hPrint,
    hPutStr,
    openFile,
    stdin,
    stdout,
  )

data Unpacker = forall a. (Eq a) => AnyUnpacker (SchemeVal -> ThrowsError a)

type Mode = String

type Primitive = [SchemeVal] -> ThrowsError SchemeVal

type IOPrimitive = [SchemeVal] -> IOThrowsError SchemeVal

--- Evaluation Functions ---

eval :: Env -> SchemeVal -> IOThrowsError SchemeVal
eval _ val@(Number _) = return val
eval _ val@(Float _) = return val
eval _ val@(Ratio x)
  | x == 0 = return $ Number 0
  | otherwise = return val
eval _ val@(Complex _) = return val
eval _ val@(String _) = return val
eval _ val@(Character _) = return val
eval _ val@(Bool _) = return val
eval _ val@(Vector _) = return val
eval env (Atom id') = getVar env id'
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "quasiquote", form]) = evalUnquotes form
  where
    evalUnquotes form' = case form' of
      List [Atom "unquote", form''] -> eval env form''
      List items ->
        do
          results <- traverse evalUnquotes items
          return $ List results
      _ -> return form
eval env (List [Atom "if", pred', conseq, alt]) =
  do
    result <- eval env pred'
    case result of
      Bool False -> eval env alt
      _ -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
  checkReserved var >> eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  checkReserved var >> eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params') : body')) =
  checkReserved var >> makeNormalFunc env params' body' >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params') varargs : body')) =
  checkReserved var >> makeVarArgs varargs env params' body' >>= defineVar env var
eval env (List (Atom "lambda" : List params' : body')) =
  makeNormalFunc env params' body'
eval env (List (Atom "lambda" : DottedList params' varargs : body')) =
  makeVarArgs varargs env params' body'
eval env (List (Atom "lambda" : varargs@(Atom _) : body')) =
  makeVarArgs varargs env [] body'
eval env (List (Atom "define-syntax" : List (Atom var : params') : body')) =
  makeMacro env params' body' >>= defineVar env var
eval env (List ((Atom "cond") : alts)) = cond env alts
eval env form@(List (Atom "case" : key : clauses)) =
  if null clauses
    then
      throwError $
        BadSpecialForm "No true clause in case expression: " form
    else case head clauses of
      List (Atom "else" : exprs) -> mapM (eval env) exprs <&> last
      List ((List datums) : exprs) ->
        do
          result <- eval env key
          let eq = mapM (\x -> eqv [result, x]) datums
          let (Right equality) = eq {- MonadFail is needed in order for
                                       it to be written as a one-liner -}
          if Bool True `elem` equality
            then mapM (eval env) exprs <&> last
            else eval env $ List (Atom "case" : key : tail clauses)
      _ -> throwError $ BadSpecialForm "Ill-formed case expression: " form
eval env (List [Atom "load", String filename]) = load filename >>= fmap last . mapM (eval env)
eval env (List (function : args)) =
  do
    func <- eval env function
    case func of
      Func {isMacro = True} -> apply func args >>= eval env
      _ -> mapM (eval env) args >>= apply func
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: SchemeVal -> [SchemeVal] -> IOThrowsError SchemeVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func _ params' varargs body' closure') args =
  if num params' /= num args && isNothing varargs
    then throwError $ NumArgs (num params') args
    else liftIO (bindVars closure' $ zip params' args) >>= bindVarArgs varargs >>= evalBody
  where
    remainingArgs = drop (length params') args
    num = toInteger . length
    evalBody env = last <$> mapM (eval env) body'
    bindVarArgs arg env =
      case arg of
        Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
        Nothing -> return env
apply (IOFunc func) args = func args
apply badForm _ = throwError $ NotFunction "Expected function, found" (show badForm)

--- Primitive Functions ---

primitives :: [(String, Primitive)]
primitives =
  [ ("+", numAdd),
    ("-", numSub),
    ("*", numMul),
    ("/", numDivide),
    ("mod", numMod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    ("abs", numAbs),
    ("ceiling", numCeil),
    ("floor", numFloor),
    ("round", numRound),
    ("truncate", numTruncate),
    ("exact?", unaryOp exactp),
    ("inexact?", unaryOp inexactp),
    ("exact->inexact", toInexact),
    ("inexact->exact", toExact),
    ("real-part", cRealPart),
    ("imag-part", cImagPart),
    ("magnitude", cMagnitude),
    ("make-polar", cMakePolar),
    ("sqrt", sciSqrt),
    ("exp", sciExp),
    ("expt", sciExpt),
    ("ln", sciLn),
    ("log", sciLog),
    ("acos", sciAcos),
    ("asin", sciAsin),
    ("atan", sciAtan),
    ("cos", sciCos),
    ("sin", sciSin),
    ("tan", sciTan),
    ("asinh", sciAsinh),
    ("acosh", sciAcosh),
    ("atanh", sciAtanh),
    ("cosh", sciCosh),
    ("sinh", sciSinh),
    ("tanh", sciTanh),
    ("number->string", numToString),
    ("symbol?", unaryOp symbolp),
    ("char?", unaryOp charp),
    ("string?", unaryOp stringp),
    ("number?", unaryOp numberp),
    ("float?", unaryOp floatp),
    ("complex?", unaryOp complexp),
    ("ratio?", unaryOp ratiop),
    ("bool?", unaryOp boolp),
    ("list?", unaryOp listp),
    ("vector?", unaryOp vectorp),
    ("procedure?", unaryOp procedurep),
    ("char-alphabetic?", charIsAlpha),
    ("char-numeric?", charIsNumeric),
    ("char-whitespace?", charIsWhitespace),
    ("char-upper-case?", charIsUpper),
    ("char-lower-case?", charIsLower),
    ("char-is-both?", charIsBoth),
    ("char-upcase", charToUpper),
    ("char-downcase", charToLower),
    ("char=?", charBoolBinop (==)),
    ("char<?", charBoolBinop (<)),
    ("char>?", charBoolBinop (>)),
    ("char<=?", charBoolBinop (<=)),
    ("char>=?", charBoolBinop (>=)),
    ("char-ci=?", charBoolBinop' (==)),
    ("char-ci<?", charBoolBinop' (<)),
    ("char-ci>?", charBoolBinop' (>)),
    ("char-ci<=?", charBoolBinop' (<=)),
    ("char-ci>=?", charBoolBinop' (>=)),
    ("symbol->string", unaryOp symbol2string),
    ("string->symbol", unaryOp string2symbol),
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("/=", numBoolBinop (/=)),
    (">=", numBoolBinop (>=)),
    ("<=", numBoolBinop (<=)),
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    ("string=?", strBoolBinop (==)),
    ("string<?", strBoolBinop (<)),
    ("string>?", strBoolBinop (>)),
    ("string<=?", strBoolBinop (<=)),
    ("string>=?", strBoolBinop (>=)),
    ("string-ci=?", strBoolBinop' (==)),
    ("string-ci<?", strBoolBinop' (<)),
    ("string-ci>?", strBoolBinop' (>)),
    ("string-ci<=?", strBoolBinop' (<=)),
    ("string-ci>=?", strBoolBinop' (>=)),
    ("string-len", stringLen),
    ("string-ref", stringRef),
    ("substring", subString),
    ("string-append", stringAppend),
    ("string", string),
    ("string-fill!", stringFill),
    ("list->string", unaryOp list2string),
    ("string->list", unaryOp string2list),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("first", car),
    ("last", lastBlock),
    ("list-ref", listRef),
    ("list-tail", listTail),
    ("list-head", listHead),
    ("eq?", eqv),
    ("eqv?", eqv),
    ("equal?", equal),
    ("vector-length", vectorLength),
    ("vector-ref", vectorRef),
    ("list->vector", list2vector),
    ("make-vector", makeVector),
    ("vector->list", vector2list)
  ]

ioPrimitives :: [(String, IOPrimitive)]
ioPrimitives =
  [ ("apply", applyProc),
    ("open-input-file", makePort ReadMode),
    ("open-output-file", makePort WriteMode),
    ("close-input-port", closePort),
    ("close-output-port", closePort),
    ("read", readProc),
    ("write", writeProc),
    ("read-contents", readContents),
    ("read-all", readAll),
    ("print", printobj),
    ("display", printobj),
    ("write-char", printobj),
    ("println", println),
    ("newline", newline),
    ("output-port?", isOutputPort),
    ("input-port?", isInputPort),
    ("port?", isPort)
  ]

primitiveBindings :: IO Env
primitiveBindings =
  nullEnv
    >>= flip
      bindVars
      ( map (makeFunc' IOFunc) ioPrimitives
          ++ map (makeFunc' PrimitiveFunc) primitives
      )
  where
    makeFunc' constructor (var, func) = (var, constructor func)

--- Numeric Functions ---

numAdd :: [SchemeVal] -> ThrowsError SchemeVal
numAdd [] = throwError $ NumArgs 2 []
numAdd val@[_] = throwError $ NumArgs 2 val
numAdd l = foldl1M (\x y -> numCast [x, y] >>= go) l
  where
    go (List [Number x, Number y]) = return $ Number $ x + y
    go (List [Float x, Float y]) = return $ Float $ x + y
    go (List [Ratio x, Ratio y]) = return $ Ratio $ x + y
    go (List [Complex x, Complex y]) = return $ Complex $ x + y
    go (List [Number x, Float y]) = return $ Float $ fromInteger x + y
    go (List [Number x, Ratio y]) = return $ Ratio $ fromInteger x + y
    go (List [Number x, Complex y]) = return $ Complex $ (fromInteger x :+ 0) + y
    go (List [Float x, Number y]) = return $ Float $ x + fromInteger y
    go (List [Ratio x, Number y]) = return $ Ratio $ x + fromInteger y
    go (List [Complex x, Number y]) = return $ Complex $ x + (fromInteger y :+ 0)
    go (List [Float x, Ratio y]) = return $ Float $ x + realToFrac y
    go (List [Float x, Complex y]) = return $ Complex $ (x :+ 0) + y
    go (List [Ratio x, Float y]) = return $ Float $ realToFrac x + y
    go (List [Complex x, Float y]) = return $ Complex $ x + (y :+ 0)
    go (List [Ratio x, Complex y]) = return $ Complex $ fromRational x + y
    go (List [Complex x, Ratio y]) = return $ Complex $ x + fromRational y
    go _ = throwError $ Default "Unexpected error in (+)"

numSub :: [SchemeVal] -> ThrowsError SchemeVal
numSub [] = throwError $ NumArgs 1 []
numSub [Number a] = return . Number $ -1 * a
numSub [Float a] = return . Float $ -1 * a
numSub [Ratio a] = return . Ratio $ -1 * a
numSub [Complex a] = return . Complex $ -1 * a
numSub l = foldl1M (\x y -> numCast [x, y] >>= go) l
  where
    go (List [Number x, Number y]) = return $ Number $ x - y
    go (List [Float x, Float y]) = return $ Float $ x - y
    go (List [Ratio x, Ratio y]) = return $ Ratio $ x - y
    go (List [Complex x, Complex y]) = return $ Complex $ x - y
    go (List [Number x, Float y]) = return $ Float $ fromInteger x - y
    go (List [Number x, Ratio y]) = return $ Ratio $ fromInteger x - y
    go (List [Number x, Complex y]) = return $ Complex $ (fromInteger x :+ 0) - y
    go (List [Float x, Number y]) = return $ Float $ x - fromInteger y
    go (List [Ratio x, Number y]) = return $ Ratio $ x - fromInteger y
    go (List [Complex x, Number y]) = return $ Complex $ x - (fromInteger y :+ 0)
    go (List [Float x, Ratio y]) = return $ Float $ x - realToFrac y
    go (List [Float x, Complex y]) = return $ Complex $ (x :+ 0) - y
    go (List [Ratio x, Float y]) = return $ Float $ realToFrac x - y
    go (List [Complex x, Float y]) = return $ Complex $ x - (y :+ 0)
    go (List [Ratio x, Complex y]) = return $ Complex $ fromRational x - y
    go (List [Complex x, Ratio y]) = return $ Complex $ x - fromRational y
    go _ = throwError $ Default "Unexpected error in (-)"

numMul :: [SchemeVal] -> ThrowsError SchemeVal
numMul [] = throwError $ NumArgs 2 []
numMul l = foldl1M (\x y -> numCast [x, y] >>= go) l
  where
    go (List [Number x, Number y]) = return $ Number $ x * y
    go (List [Float x, Float y]) = return $ Float $ x * y
    go (List [Ratio x, Ratio y]) = return $ Ratio $ x * y
    go (List [Complex x, Complex y]) = return $ Complex $ x * y
    go (List [Number x, Float y]) = return $ Float $ fromInteger x * y
    go (List [Number x, Ratio y]) = return $ Ratio $ fromInteger x * y
    go (List [Number x, Complex y]) = return $ Complex $ (fromInteger x :+ 0) * y
    go (List [Float x, Number y]) = return $ Float $ x * fromInteger y
    go (List [Ratio x, Number y]) = return $ Ratio $ x * fromInteger y
    go (List [Complex x, Number y]) = return $ Complex $ x * (fromInteger y :+ 0)
    go (List [Float x, Ratio y]) = return $ Float $ x * realToFrac y
    go (List [Float x, Complex y]) = return $ Complex $ (x :+ 0) * y
    go (List [Ratio x, Float y]) = return $ Float $ realToFrac x * y
    go (List [Complex x, Float y]) = return $ Complex $ x * (y :+ 0)
    go (List [Ratio x, Complex y]) = return $ Complex $ fromRational x * y
    go (List [Complex x, Ratio y]) = return $ Complex $ x * fromRational y
    go _ = throwError $ Default "Unexpected error in (*)"

numDivide :: [SchemeVal] -> ThrowsError SchemeVal
numDivide [] = throwError $ NumArgs 1 []
numDivide [Number 0] = throwError DivideByZero
numDivide [Ratio 0] = throwError DivideByZero
numDivide [Number x] = return $ Ratio $ 1 / fromInteger x
numDivide [Float x] = return $ Float $ 1.0 / x
numDivide [Ratio x] = return $ Ratio $ 1 / x
numDivide [Complex x] = return $ Complex $ 1 / x
numDivide l = foldl1M (\x y -> numCast [x, y] >>= go) l
  where
    go (List [Number x, Number y])
      | y == 0 = throwError DivideByZero
      | mod x y == 0 = return $ Number $ div x y
      | otherwise = return $ Ratio $ fromInteger x / fromInteger y
    go (List [Float x, Float y])
      | y == 0 = throwError DivideByZero
      | otherwise = return $ Float $ x / y
    go (List [Ratio x, Ratio y])
      | y == 0 = throwError DivideByZero
      | otherwise = return $ Ratio $ x / y
    go (List [Complex x, Complex y])
      | y == 0 = throwError DivideByZero
      | otherwise = return $ Complex $ x / y
    go _ = throwError $ Default "Unexpected error in (/)"

numMod :: [SchemeVal] -> ThrowsError SchemeVal
numMod [] = throwError $ NumArgs 1 []
numMod l = foldl1M (\x y -> numCast [x, y] >>= go) l
  where
    go (List [Number a, Number b]) = return $ Number $ mod' a b
    go (List [Float a, Float b]) = return $ Float $ mod' a b
    go (List [Ratio a, Ratio b]) = return $ Ratio $ mod' a b
    go (List [Complex _, Complex _]) =
      throwError $
        Default "Modulus is not yet implemented for complex numbers"
    go _ = throwError $ Default "Unexpected error in (modulus)"

numAbs :: [SchemeVal] -> ThrowsError SchemeVal
numAbs [Number x] = return $ Number $ abs x
numAbs [Float x] = return $ Float $ abs x
numAbs [Complex x] = return $ Complex $ abs x
numAbs [Ratio x] = return $ Ratio $ abs x
numAbs [x] = throwError $ TypeMismatch "Number" x
numAbs l = throwError $ NumArgs 1 l

numCeil, numFloor, numRound, numTruncate :: [SchemeVal] -> ThrowsError SchemeVal
numCeil [Number x] = return $ Number $ ceiling $ fromInteger x
numCeil [Ratio x] = return $ Number $ ceiling $ fromRational x
numCeil [Float x] = return $ Number $ ceiling x
numCeil [Complex x] =
  if imagPart x == 0
    then return $ Number $ ceiling $ realPart x
    else throwError $ TypeMismatch "Integer or Float" $ Complex x
numCeil [x] = throwError $ TypeMismatch "Integer or Float" x
numCeil l = throwError $ NumArgs 1 l
numFloor [Number x] = return $ Number $ floor $ fromInteger x
numFloor [Ratio x] = return $ Number $ floor $ fromRational x
numFloor [Float x] = return $ Number $ floor x
numFloor [Complex x] =
  if imagPart x == 0
    then return $ Number $ floor $ realPart x
    else throwError $ TypeMismatch "Integer or Float" $ Complex x
numFloor [x] = throwError $ TypeMismatch "Integer or Float" x
numFloor l = throwError $ NumArgs 1 l
numRound [Number x] = return $ Number $ round $ fromInteger x
numRound [Ratio x] = return $ Number $ round $ fromRational x
numRound [Float x] = return $ Number $ round x
numRound [Complex x] =
  if imagPart x == 0
    then return $ Number $ round $ realPart x
    else throwError $ TypeMismatch "Integer or Float" $ Complex x
numRound [x] = throwError $ TypeMismatch "Integer or Float" x
numRound l = throwError $ NumArgs 1 l
numTruncate [Number x] = return $ Number $ truncate $ fromInteger x
numTruncate [Ratio x] = return $ Number $ truncate $ fromRational x
numTruncate [Float x] = return $ Number $ truncate x
numTruncate [Complex x] =
  if imagPart x == 0
    then return $ Number $ truncate $ realPart x
    else throwError $ TypeMismatch "Integer or Float" $ Complex x
numTruncate [x] = throwError $ TypeMismatch "Integer or Float" x
numTruncate l = throwError $ NumArgs 1 l

toExact :: [SchemeVal] -> ThrowsError SchemeVal
toExact [x@(Number _)] = return x
toExact [Ratio x] =
  if denominator x == 1
    then return $ Number $ numerator x
    else return $ Ratio x
toExact [Float x] = toExact [Ratio (toRational x)]
toExact [Complex x] =
  if imagPart x == 0
    then toExact [Ratio (toRational (realPart x))]
    else throwError $ TypeMismatch "Exact complex" $ Complex x
toExact [notNum] = throwError $ TypeMismatch "Number" notNum
toExact badArgList = throwError $ NumArgs 1 badArgList

toInexact :: [SchemeVal] -> ThrowsError SchemeVal
toInexact [Number x] = return $ Float $ fromInteger x
toInexact [Ratio x] = return $ Float $ fromRational x
toInexact [x@(Float _)] = return x
toInexact [Complex x] =
  if imagPart x == 0
    then return $ Float $ realPart x
    else return $ Complex x
toInexact [notNum] = throwError $ TypeMismatch "Number" notNum
toInexact badArgList = throwError $ NumArgs 1 badArgList

cRealPart, cImagPart, cMakePolar, cMagnitude :: [SchemeVal] -> ThrowsError SchemeVal
cRealPart [Number x] = return $ Number $ fromInteger x
cRealPart [Float x] = return $ Float x
cRealPart [Ratio x] = return $ Float $ fromRational x
cRealPart [Complex x] = return $ Float $ realPart x
cRealPart [notnum] = throwError $ TypeMismatch "Number" notnum
cRealPart badArgList = throwError $ NumArgs 1 badArgList
cImagPart [Number _] = return $ Number 0
cImagPart [Float _] = return $ Number 0
cImagPart [Ratio _] = return $ Number 0
cImagPart [Complex x] = return $ Float $ imagPart x
cImagPart [notnum] = throwError $ TypeMismatch "Number" notnum
cImagPart badArgList = throwError $ NumArgs 1 badArgList
cMakePolar [mag, p] = numCast [mag, p] >>= go
  where
    go (List [Number mag', Number p'])
      | mag' == 0 = return $ Number 0
      | p' == 0 = return $ Number mag'
      | otherwise = return $ Complex $ mkPolar (fromInteger mag') (fromInteger p')
    go (List [Float mag', Float p'])
      | mag' == 0 = return $ Number 0
      | p' == 0 = return $ Float mag'
      | otherwise = return $ Complex $ mkPolar mag' p'
    go (List [Ratio mag', Ratio p'])
      | mag' == 0 = return $ Number 0
      | p' == 0 = return $ Float (fromRational mag')
      | otherwise = return $ Complex $ mkPolar (fromRational mag') (fromRational p')
    go val@(List [Complex _, Complex _]) = throwError $ TypeMismatch "Real" val
    go _ = throwError $ Default "Unexpected error in make-polar"
cMakePolar badArgList = throwError $ NumArgs 2 badArgList
cMagnitude [Number x] = return $ Number $ fromInteger x
cMagnitude [Float x] = return $ Float x
cMagnitude [Ratio x] = return $ Float $ fromRational x
cMagnitude [Complex x] = return $ Float $ magnitude x
cMagnitude [notnum] = throwError $ TypeMismatch "Number" notnum
cMagnitude badArgList = throwError $ NumArgs 1 badArgList

sciSqrt, sciExp, sciExpt, sciLn, sciLog :: [SchemeVal] -> ThrowsError SchemeVal
sciSqrt [Number a] = return $ Float $ sqrt $ fromInteger a
sciSqrt [Float a] = return $ Float $ sqrt a
sciSqrt [Ratio a] = return $ Float $ sqrt $ fromRational a
sciSqrt [Complex x] = return $ Complex $ sqrt x
sciSqrt [notnum] = throwError $ TypeMismatch "Number" notnum
sciSqrt badArgList = throwError $ NumArgs 1 badArgList
sciExp [Number x] = return $ Float $ exp $ fromInteger x
sciExp [Float x] = return $ Float $ exp x
sciExp [Ratio x] = return $ Float $ exp $ fromRational x
sciExp [Complex x] = return $ Complex $ exp x
sciExp [notnum] = throwError $ TypeMismatch "Number" notnum
sciExp badArgList = throwError $ NumArgs 1 badArgList
sciExpt [x, y] = numCast [x, y] >>= go
  where
    go (List [Number x', Number y']) =
      return $
        Number $
          round $
            fromInteger x' ** fromInteger y'
    go (List [Float x', Float y']) = return $ Float $ x' ** y'
    go (List [Ratio x', Ratio y']) = return $ Float $ fromRational x' ** fromRational y'
    go (List [Complex x', Complex y']) = return $ Complex $ x' ** y'
    go _ = throwError $ Default "Unexpected error in (expt)"
sciExpt badArgList = throwError $ NumArgs 2 badArgList
sciLn [Number x] = return $ Float $ log $ fromInteger x
sciLn [Float x] = return $ Float $ log x
sciLn [Ratio x] = return $ Float $ log $ fromRational x
sciLn [Complex x] = return $ Complex $ log x
sciLn [notnum] = throwError $ TypeMismatch "Number" notnum
sciLn badArgList = throwError $ NumArgs 1 badArgList
sciLog [Number x] = return $ Float $ logBase 10 $ fromInteger x
sciLog [Float x] = return $ Float $ logBase 10 x
sciLog [Ratio x] = return $ Float $ logBase 10 $ fromRational x
sciLog [Complex x] = return $ Complex $ logBase 10 x
sciLog [notnum] = throwError $ TypeMismatch "Number" notnum
sciLog badArgList = throwError $ NumArgs 1 badArgList

sciCos, sciSin, sciTan, sciAcos, sciAsin, sciAtan :: [SchemeVal] -> ThrowsError SchemeVal
sciCos [Number x] = return $ Float $ cos $ fromInteger x
sciCos [Float x] = return $ Float $ cos x
sciCos [Ratio x] = return $ Float $ cos $ fromRational x
sciCos [Complex x] = return $ Complex $ cos x
sciCos [notnum] = throwError $ TypeMismatch "Number" notnum
sciCos badArgList = throwError $ NumArgs 1 badArgList
sciSin [Number x] = return $ Float $ sin $ fromInteger x
sciSin [Float x] = return $ Float $ sin x
sciSin [Ratio x] = return $ Float $ sin $ fromRational x
sciSin [Complex x] = return $ Complex $ sin x
sciSin [notnum] = throwError $ TypeMismatch "Number" notnum
sciSin badArgList = throwError $ NumArgs 1 badArgList
sciTan [Number x] = return $ Float $ tan $ fromInteger x
sciTan [Float x] = return $ Float $ tan x
sciTan [Ratio x] = return $ Float $ tan $ fromRational x
sciTan [Complex x] = return $ Complex $ tan x
sciTan [notnum] = throwError $ TypeMismatch "Number" notnum
sciTan badArgList = throwError $ NumArgs 1 badArgList
sciAcos [Number x] = return $ Float $ acos $ fromInteger x
sciAcos [Float x] = return $ Float $ acos x
sciAcos [Ratio x] = return $ Float $ acos $ fromRational x
sciAcos [Complex x] = return $ Complex $ acos x
sciAcos [notnum] = throwError $ TypeMismatch "Number" notnum
sciAcos badArgList = throwError $ NumArgs 1 badArgList
sciAsin [Number x] = return $ Float $ asin $ fromInteger x
sciAsin [Float x] = return $ Float $ asin x
sciAsin [Ratio x] = return $ Float $ asin $ fromRational x
sciAsin [Complex x] = return $ Complex $ asin x
sciAsin [notnum] = throwError $ TypeMismatch "Number" notnum
sciAsin badArgList = throwError $ NumArgs 1 badArgList
sciAtan [Number x] = return $ Float $ atan $ fromInteger x
sciAtan [Float x] = return $ Float $ atan x
sciAtan [Ratio x] = return $ Float $ atan $ fromRational x
sciAtan [Complex x] = return $ Complex $ atan x
sciAtan [notnum] = throwError $ TypeMismatch "Number" notnum
sciAtan badArgList = throwError $ NumArgs 1 badArgList

sciAcosh, sciAsinh, sciAtanh, sciCosh, sciSinh, sciTanh :: [SchemeVal] -> ThrowsError SchemeVal
sciAsinh [Number x] = return $ Float $ asinh $ fromInteger x
sciAsinh [Float x] = return $ Float $ asinh x
sciAsinh [Ratio x] = return $ Float $ asinh $ fromRational x
sciAsinh [Complex x] = return $ Complex $ asinh x
sciAsinh [notnum] = throwError $ TypeMismatch "Number" notnum
sciAsinh badArgList = throwError $ NumArgs 1 badArgList
sciAcosh [Number x] =
  if x < 1
    then return $ Complex $ acosh $ fromInteger x :+ 0
    else return $ Float $ acosh $ fromInteger x
sciAcosh [Float x] =
  if x < 1
    then return $ Complex $ acosh x :+ 0
    else return $ Float $ acosh x
sciAcosh [Ratio x] = return $ Float $ acos $ fromRational x
sciAcosh [Complex x] = return $ Complex $ acos x
sciAcosh [notnum] = throwError $ TypeMismatch "Number" notnum
sciAcosh badArgList = throwError $ NumArgs 1 badArgList
sciAtanh [Number x] = return $ Float $ atanh $ fromInteger x
sciAtanh [Float x] = return $ Float $ atanh x
sciAtanh [Ratio x] = return $ Float $ atanh $ fromRational x
sciAtanh [Complex x] = return $ Complex $ atanh x
sciAtanh [notnum] = throwError $ TypeMismatch "Number" notnum
sciAtanh badArgList = throwError $ NumArgs 1 badArgList
sciCosh [Number x] = return $ Float $ cosh $ fromInteger x
sciCosh [Float x] = return $ Float $ cosh x
sciCosh [Ratio x] = return $ Float $ cosh $ fromRational x
sciCosh [Complex x] = return $ Complex $ cosh x
sciCosh [notnum] = throwError $ TypeMismatch "Number" notnum
sciCosh badArgList = throwError $ NumArgs 1 badArgList
sciSinh [Number x] = return $ Float $ sinh $ fromInteger x
sciSinh [Float x] = return $ Float $ sinh x
sciSinh [Ratio x] = return $ Float $ sinh $ fromRational x
sciSinh [Complex x] = return $ Complex $ sinh x
sciSinh [notnum] = throwError $ TypeMismatch "Number" notnum
sciSinh badArgList = throwError $ NumArgs 1 badArgList
sciTanh [Number x] = return $ Float $ tanh $ fromInteger x
sciTanh [Float x] = return $ Float $ tanh x
sciTanh [Ratio x] = return $ Float $ tanh $ fromRational x
sciTanh [Complex x] = return $ Complex $ tanh x
sciTanh [notnum] = throwError $ TypeMismatch "Number" notnum
sciTanh badArgList = throwError $ NumArgs 1 badArgList

numToString :: [SchemeVal] -> ThrowsError SchemeVal
numToString [Number x] = return $ String $ show x
numToString [Float x] = return $ String $ show x
numToString [Ratio x] = return $ String $ show x
numToString [Complex x] = return $ String $ show x
numToString [x] = throwError $ TypeMismatch "number" x
numToString badArgList = throwError $ NumArgs 2 badArgList

-- OPs

unaryOp :: (SchemeVal -> SchemeVal) -> Primitive
unaryOp f [v] = return (f v)
unaryOp _ [] = throwError $ NumArgs 1 []
unaryOp _ badArgList = throwError $ NumArgs 1 badArgList

numericBinop :: (Integer -> Integer -> Integer) -> Primitive
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params' = mapM unpackNum params' <&> (Number . foldl1 op)

boolBinop :: (SchemeVal -> ThrowsError a) -> (a -> a -> Bool) -> Primitive
boolBinop unpacker op args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else do
      left <- unpacker $ head args
      right <- unpacker $ args !! 1
      return $ Bool $ left `op` right

unpackNum :: SchemeVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n
   in if null parsed
        then throwError $ TypeMismatch "Number" $ String n
        else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "Number" notNum

unpackStr :: Mode -> SchemeVal -> ThrowsError String
unpackStr " " (String s) = return s
unpackStr " " (Number s) = return $ show s
unpackStr " " (Bool s) = return $ show s
unpackStr "ci" (String s) = return $ map toLower s
unpackStr "ci" (Number s) = return $ map toLower $ show s
unpackStr "ci" (Bool s) = return $ map toLower $ show s
unpackStr _ notString = throwError $ TypeMismatch "String" notString

unpackBool :: SchemeVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "Bool" notBool

unpackChar :: Mode -> SchemeVal -> ThrowsError Char
unpackChar " " (Character c) = return c
unpackChar "ci" (Character c) = return $ toLower c
unpackChar _ notChar = throwError $ TypeMismatch "Char" notChar

numBoolBinop :: (Integer -> Integer -> Bool) -> Primitive
numBoolBinop = boolBinop unpackNum

strBoolBinop, strBoolBinop' :: (String -> String -> Bool) -> Primitive
strBoolBinop = boolBinop (unpackStr " ")
strBoolBinop' = boolBinop (unpackStr "ci")

boolBoolBinop :: (Bool -> Bool -> Bool) -> Primitive
boolBoolBinop = boolBinop unpackBool

charBoolBinop, charBoolBinop' :: (Char -> Char -> Bool) -> Primitive
charBoolBinop = boolBinop (unpackChar " ")
charBoolBinop' = boolBinop (unpackChar "ci")

symbolp,
  numberp,
  floatp,
  ratiop,
  complexp,
  charp,
  stringp,
  boolp,
  listp,
  exactp,
  inexactp,
  vectorp,
  procedurep ::
    SchemeVal -> SchemeVal
symbolp (Atom _) = Bool True
symbolp _ = Bool False
numberp (Number _) = Bool True
numberp _ = Bool False
floatp (Float _) = Bool True
floatp _ = Bool False
ratiop (Ratio _) = Bool True
ratiop _ = Bool False
complexp (Complex _) = Bool True
complexp _ = Bool False
charp (Character _) = Bool True
charp _ = Bool False
stringp (String _) = Bool True
stringp _ = Bool False
boolp (Bool _) = Bool True
boolp _ = Bool False
listp (List _) = Bool True
listp (DottedList _ _) = Bool False
listp _ = Bool False
exactp (Complex c)
  | imagPart c == 0 = Bool True
  | otherwise = Bool False
exactp (Float _) = Bool False
exactp (Number _) = Bool True
exactp (Ratio _) = Bool True
exactp _ = Bool False
inexactp x = Bool $ let (Bool a) = exactp x in not a
vectorp (Vector _) = Bool True
vectorp _ = Bool False
procedurep (IOFunc _) = Bool True
procedurep (Func {}) = Bool True
procedurep (PrimitiveFunc _) = Bool True
procedurep _ = Bool False

-- Chars

charIsAlpha,
  charIsNumeric,
  charIsWhitespace,
  charIsLower,
  charIsUpper,
  charIsBoth ::
    [SchemeVal] -> ThrowsError SchemeVal
charIsAlpha [Character x] = return $ Bool $ isAlpha x
charIsAlpha [notchar] = throwError $ TypeMismatch "Char" notchar
charIsAlpha badArgList = throwError $ NumArgs 1 badArgList
charIsNumeric [Character x] = return $ Bool $ isNumber x
charIsNumeric [notchar] = throwError $ TypeMismatch "Char" notchar
charIsNumeric badArgList = throwError $ NumArgs 1 badArgList
charIsWhitespace [Character x] = return $ Bool $ isSpace x
charIsWhitespace [notchar] = throwError $ TypeMismatch "Char" notchar
charIsWhitespace badArgList = throwError $ NumArgs 1 badArgList
charIsLower [Character x] = return $ Bool $ isLower x
charIsLower [notchar] = throwError $ TypeMismatch "Char" notchar
charIsLower badArgList = throwError $ NumArgs 1 badArgList
charIsUpper [Character x] = return $ Bool $ isUpper x
charIsUpper [notchar] = throwError $ TypeMismatch "Char" notchar
charIsUpper badArgList = throwError $ NumArgs 1 badArgList
charIsBoth [Character x] = return $ Bool $ isAlpha x
charIsBoth [notchar] = throwError $ TypeMismatch "Char" notchar
charIsBoth badArgList = throwError $ NumArgs 1 badArgList

charToLower, charToUpper :: [SchemeVal] -> ThrowsError SchemeVal
charToLower [Character x] = return $ Character $ toLower x
charToLower [notchar] = throwError $ TypeMismatch "Char" notchar
charToLower badArgList = throwError $ NumArgs 1 badArgList
charToUpper [Character x] = return $ Character $ toUpper x
charToUpper [notchar] = throwError $ TypeMismatch "Char" notchar
charToUpper badArgList = throwError $ NumArgs 1 badArgList

-- symbol

symbol2string, string2symbol :: SchemeVal -> SchemeVal
symbol2string (Atom s) = String s
symbol2string _ = String ""
string2symbol (String s) = Atom s
string2symbol _ = Atom ""

-- List

car :: Primitive
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "Pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: Primitive
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "Pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: Primitive
cons [x1, List []] = return $ List [x1] -- List [] = NIL
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

-- equivalence

eqv :: Primitive
eqv [a, b] = return $ Bool $ a == b
eqv badArgList = throwError $ NumArgs 2 badArgList

unpackEquals :: SchemeVal -> SchemeVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do
    unpacked1 <- unpacker arg1
    unpacked2 <- unpacker arg2
    return (unpacked1 == unpacked2) `catchError` const (return False)

equal :: Primitive
equal [DottedList xs x, DottedList ys y] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [l1@(List _), l2@(List _)] = eqvList equal [l1, l2]
equal [arg1, arg2] =
  do
    primitiveEquals <-
      or
        <$> mapM
          (unpackEquals arg1 arg2)
          [AnyUnpacker unpackNum, AnyUnpacker (unpackStr " "), AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

eqvList :: Primitive -> Primitive
eqvList eqvFunc [List arg1, List arg2] =
  return $ Bool $ (length arg1 == length arg2) && all eqvPair (zip arg1 arg2)
  where
    eqvPair (x1, x2) = case eqvFunc [x1, x2] of
      Left _ -> False
      Right (Bool val) -> val
      _ -> undefined
eqvList _ _ = undefined

-- String

stringLen :: Primitive
stringLen [String a] = Right $ Number $ fromIntegral $ length a
stringLen [notString] = throwError $ TypeMismatch "String" notString
stringLen badArgList = throwError $ NumArgs 1 badArgList

stringRef :: Primitive
stringRef [String s, Number k]
  | length s < k' + 1 = throwError $ Default "Out of bound error"
  | otherwise = Right $ String [s !! k']
  where
    k' = fromIntegral k
stringRef [String _, notNum] = throwError $ TypeMismatch "Number" notNum
stringRef [notString, _] = throwError $ TypeMismatch "String" notString
stringRef badArgList = throwError $ NumArgs 2 badArgList

string2list, list2string :: SchemeVal -> SchemeVal
string2list (String s) = List $ map Character s
string2list _ = List [Character ' ']
list2string (List s) = String $ map (\(Character x) -> x) s
list2string _ = String ""

subString :: Primitive
subString [String s, Number start, Number end]
  | start' >= 0 && end' >= start' && l >= end' = return (String (take diff (drop start' s)))
  | start' < 0 || l < end' = throwError $ Default "Out of bound error"
  | otherwise = throwError $ Default "Start index greater than end index"
  where
    start' = fromIntegral start
    end' = fromIntegral end
    l = length s
    diff = end' - start'
subString [String _, notNum, Number _] = throwError $ TypeMismatch "Number" notNum
subString [String _, Number _, notNum] = throwError $ TypeMismatch "Number" notNum
subString [String _, notNum, _] = throwError $ TypeMismatch "Number" notNum
subString [notString, _] = throwError $ TypeMismatch "String" notString
subString badArgList = throwError $ NumArgs 3 badArgList

stringAppend :: Primitive
stringAppend [] = return $ String []
stringAppend ((String x) : xs) =
  do
    rest <- stringAppend xs -- MonadFail is needed in order for it to be written as a one-liner
    let (String rs) = rest
    return $ String $ x ++ rs
stringAppend [notString] = throwError $ TypeMismatch "String" notString
stringAppend badArgList = throwError $ NumArgs 1 badArgList

string :: Primitive
string [] = return $ String ""
string ((Character x) : xs) =
  do
    rest <- string xs -- MonadFail is needed in order for it to be written as a one-liner
    let (String rs) = rest
    return $ String $ x : rs
string [notChars] = throwError $ TypeMismatch "Character" notChars
string badArgList = throwError $ NumArgs 2 badArgList

stringFill :: Primitive
stringFill [String s, Character c] = return $ String $ map (const c) s
stringFill [String _, notChar] = throwError $ TypeMismatch "Character" notChar
stringFill [notString, _] = throwError $ TypeMismatch "String" notString
stringFill badArgList = throwError $ NumArgs 2 badArgList

-- IO

applyProc :: IOPrimitive
applyProc [func, List args] = apply func args
applyProc [func, DottedList args rest] = applyProc [func, List (args ++ [rest])]
applyProc (func : args) = apply func args
applyProc _ = undefined

makePort :: IOMode -> IOPrimitive
makePort mode [String filename] = fmap Port $ liftIO $ openFile filename mode
makePort _ _ = undefined

closePort :: IOPrimitive
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _ = return $ Bool False

readProc :: IOPrimitive
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr
readProc _ = undefined

writeProc :: IOPrimitive
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)
writeProc _ = undefined

readContents :: IOPrimitive
readContents [String filename] = fmap String $ liftIO $ readFile filename
readContents _ = undefined

load :: String -> IOThrowsError [SchemeVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

readAll :: IOPrimitive
readAll [String filename] = List <$> load filename
readAll _ = undefined

printobj :: IOPrimitive
printobj [obj] = printobj [obj, Port stdout]
printobj [String s, Port port] = liftIO $ hPutStr port s >> return (Bool True)
printobj [Character c, Port port] = liftIO $ hPutStr port (c : "") >> return (Bool True)
printobj [obj, Port port] = liftIO $ hPutStr port (show obj) >> return (Bool True)
printobj badArgList = throwError $ NumArgs 2 badArgList

newline :: IOPrimitive
newline [] = writeProc [Character '\n', Port stdout]
newline [Port port] = writeProc [Character '\n', Port port]
newline badArgList = throwError $ NumArgs 1 badArgList

println :: IOPrimitive
println = foldr (\obj -> (>>) (printobj [obj])) (newline [])

isOutputPort :: IOPrimitive
isOutputPort [Port x] = liftIO $ hIsWritable x <&> Bool
isOutputPort [notPort] = throwError $ TypeMismatch "<IO port>" notPort
isOutputPort badArgList = throwError $ NumArgs 1 badArgList

isInputPort :: IOPrimitive
isInputPort [Port x] = liftIO $ hIsReadable x <&> Bool
isInputPort [notPort] = throwError $ TypeMismatch "<IO port>" notPort
isInputPort badArgList = throwError $ NumArgs 1 badArgList

isPort :: IOPrimitive
isPort [Port _] = return $ Bool True
isPort [_] = return $ Bool False
isPort badArgList = throwError $ NumArgs 1 badArgList

-- List

listRef :: Primitive
listRef [List x, Number n] =
  let k = fromInteger n
   in if 0 <= k && k <= length x
        then return $ x !! k
        else throwError $ Default $ "index " ++ show k ++ " is out of bounds"
listRef [List _, notNum] = throwError $ TypeMismatch "Number" notNum
listRef [notList] = throwError $ TypeMismatch "List" notList
listRef badArgList = throwError $ NumArgs 1 badArgList

listTail :: Primitive
listTail [List lst, Number n] =
  let k = fromInteger n
   in if 0 <= k && k <= length lst
        then return $ List $ drop k lst
        else throwError $ Default $ "index " ++ show k ++ " is out of bounds"
listTail [List _, notNum] = throwError $ TypeMismatch "Number" notNum
listTail [notList] = throwError $ TypeMismatch "List" notList
listTail badArgList = throwError $ NumArgs 1 badArgList

listHead :: Primitive
listHead [List lst, Number n] =
  let k = fromInteger n
   in if 0 <= k && k <= length lst
        then return $ List $ take k lst
        else throwError $ Default $ "index " ++ show k ++ " is out of bounds"
listHead [List _, notNum] = throwError $ TypeMismatch "Number" notNum
listHead [notList] = throwError $ TypeMismatch "List" notList
listHead badArgList = throwError $ NumArgs 1 badArgList

lastBlock :: Primitive
lastBlock [l@(List [])] = return l
lastBlock [List x] = return $ last x
lastBlock [notList] = throwError $ TypeMismatch "List" notList
lastBlock badArgList = throwError $ NumArgs 1 badArgList

-- Vector

list2vector, vector2list :: Primitive
list2vector [List x] = return $ Vector $ listArray (0, length x - 1) x
list2vector [notList] = throwError $ TypeMismatch "List" notList
list2vector badArgList = throwError $ NumArgs 1 badArgList
vector2list [Vector x] = return $ List $ elems x
vector2list [notVect] = throwError $ TypeMismatch "vector" notVect
vector2list badArgList = throwError $ NumArgs 1 badArgList

makeVector :: Primitive
makeVector [Number k] =
  return $
    Vector $
      listArray (0, fromInteger k - 1) $
        replicate (fromInteger k) (List [])
makeVector [Number k, fill] =
  return $
    Vector $
      listArray (0, fromInteger k - 1) $
        replicate (fromInteger k) fill
makeVector (notNum : _) = throwError $ TypeMismatch "number" notNum
makeVector badArgList = throwError $ NumArgs 1 badArgList

vectorLength :: Primitive
vectorLength [Vector x] = return $ Number $ toInteger $ 1 + snd (bounds x)
vectorLength [notVect] = throwError $ TypeMismatch "vector" notVect
vectorLength badArgList = throwError $ NumArgs 1 badArgList

vectorRef :: Primitive
vectorRef [Vector x, Number n] =
  let k = fromInteger n
   in if fst (bounds x) <= k && k <= snd (bounds x)
        then return $ x ! k
        else throwError $ Default $ "index " ++ show k ++ " is out of bounds"
vectorRef [Vector _, notNum] = throwError $ TypeMismatch "number" notNum
vectorRef [notVect] = throwError $ TypeMismatch "vector" notVect
vectorRef badArgList = throwError $ NumArgs 1 badArgList

-- Helper

foldl1M :: (Monad m) => (a -> a -> m a) -> [a] -> m a
foldl1M f (x : xs) = foldlM f x xs
foldl1M _ _ = error "Unexpected error in foldl1M"

cond :: Env -> IOPrimitive
cond env [List [Atom "else", value]] = eval env value
cond env ((List [condition, value]) : alts) =
  do
    result <- eval env condition
    let boolResult = unpackBool result
    let (Right bool) = boolResult
    if bool
      then eval env value
      else cond env alts
cond _ ((List a) : _) = throwError $ NumArgs 2 a
cond _ (a : _) = throwError $ NumArgs 2 [a]
cond _ _ = throwError $ Default "Not viable alternative in cond"

numCast :: [SchemeVal] -> ThrowsError SchemeVal
numCast [a@(Number _), b@(Number _)] = return $ List [a, b]
numCast [a@(Float _), b@(Float _)] = return $ List [a, b]
numCast [a@(Ratio _), b@(Ratio _)] = return $ List [a, b]
numCast [a@(Complex _), b@(Complex _)] = return $ List [a, b]
numCast [Number a, b@(Float _)] = return $ List [Float $ fromInteger a, b]
numCast [Number a, b@(Ratio _)] = return $ List [Ratio $ fromInteger a, b]
numCast [Number a, b@(Complex _)] = return $ List [Complex $ fromInteger a, b]
numCast [a@(Float _), Number b] = return $ List [a, Float $ fromInteger b]
numCast [a@(Float _), Ratio b] = return $ List [a, Float $ fromRational b]
numCast [Float a, b@(Complex _)] = return $ List [Complex $ a :+ 0, b]
numCast [a@(Ratio _), Number b] = return $ List [a, Ratio $ fromInteger b]
numCast [Ratio a, b@(Float _)] = return $ List [Float $ fromRational a, b]
numCast [Ratio a, b@(Complex _)] =
  return $
    List [Complex $ fromInteger (numerator a) / fromInteger (denominator a), b]
numCast [a@(Complex _), Number b] = return $ List [a, Complex $ fromInteger b]
numCast [a@(Complex _), Float b] = return $ List [a, Complex $ b :+ 0]
numCast [a@(Complex _), Ratio b] =
  return $
    List [a, Complex $ fromInteger (numerator b) / fromInteger (denominator b)]
numCast [a, b] = case a of
  Number _ -> throwError $ TypeMismatch "Number" b
  Float _ -> throwError $ TypeMismatch "Number" b
  Ratio _ -> throwError $ TypeMismatch "Number" b
  Complex _ -> throwError $ TypeMismatch "Number" b
  _ -> throwError $ TypeMismatch "Number" a
numCast _ = throwError $ Default "Unknown error in numCast"

reservedKeywords :: [String]
reservedKeywords = ["define", "load", "if", "cond", "case", "apply", "lambda", "set!"]

checkReserved :: String -> IOThrowsError ()
checkReserved var
  | var `elem` reservedKeywords = throwError $ ReservedKeyword var
  | otherwise = case lookup var primitives of
      Just _ -> throwError $ ReservedKeyword var
      Nothing -> return ()
