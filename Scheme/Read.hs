{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Read where

import Control.Monad (void)
import Control.Monad.Except (MonadError (throwError), void)
import Data.Array (listArray)
import Data.Complex (Complex ((:+)))
import Data.Ratio ((%))
import Env
  ( SchemeError (Parser),
    SchemeVal
      ( Atom,
        Bool,
        Character,
        Complex,
        DottedList,
        Float,
        List,
        Number,
        Ratio,
        String,
        Vector
      ),
    ThrowsError,
  )
import Numeric (readFloat, readHex, readOct)
import Text.ParserCombinators.Parsec
  ( Parser,
    alphaNum,
    anyChar,
    char,
    digit,
    endBy,
    eof,
    hexDigit,
    letter,
    many,
    many1,
    manyTill,
    noneOf,
    notFollowedBy,
    octDigit,
    oneOf,
    optionMaybe,
    parse,
    sepEndBy,
    skipMany,
    skipMany1,
    space,
    string,
    tab,
    try,
    (<|>),
  )

-- | Rifare con Megaparsec
readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

mainParser :: Parser SchemeVal
mainParser = do
  skipMany whitespace
  result <- parseExpr
  skipMany whitespace
  return result

readExpr :: String -> ThrowsError SchemeVal
readExpr = readOrThrow mainParser

readExprList :: String -> ThrowsError [SchemeVal]
readExprList = readOrThrow $ many1 mainParser

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseExpr :: Parser SchemeVal
parseExpr =
  try parseComplex
    <|> try parseRatio
    <|> try parseFloat
    <|> try parseNumber
    <|> try parseAtom
    <|> parseString
    <|> try parseBool
    <|> try parseCharacter
    <|> try parseQuoted
    <|> try parseQuasiQuoted
    <|> try parseUnQuote
    <|> try parseVector
    <|> try parseParens

parseAtom :: Parser SchemeVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  return $ Atom (first : rest)

escapedChars :: Parser Char
escapedChars = do
  char '\\'
  x <- oneOf "\\\"nrt"
  return $ case x of
    '\\' -> x
    '"' -> x
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    _ -> 'f'

parseString :: Parser SchemeVal
parseString = do
  char '"'
  x <- many $ escapedChars <|> noneOf "\"\\"
  char '"'
  return $ String x

parseFloat :: Parser SchemeVal
parseFloat = do
  sign <- parseDoubleSign
  x <- many1 digit
  char '.'
  y <- many1 digit
  (return . Float . (* sign) . fst . head . readFloat) $ x ++ "." ++ y

parseNumber :: Parser SchemeVal
parseNumber = parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin

parseIntegerSign :: Parser Integer
parseIntegerSign = do
  signChar <- optionMaybe $ oneOf "+-"
  return $ case signChar of
    Just '-' -> -1
    Just '+' -> 1
    _ -> 1

parseDoubleSign :: Parser Double
parseDoubleSign = do
  signChar <- optionMaybe $ oneOf "+-"
  return $ case signChar of
    Just '-' -> -1
    Just '+' -> 1
    _ -> 1

parseDoubleSignStrict :: Parser Double
parseDoubleSignStrict = do
  signChar <- optionMaybe $ oneOf "+-"
  case signChar of
    Just '-' -> return $ -1
    Just '+' -> return 1
    _ -> fail "no sign"

parseDecimal1 :: Parser SchemeVal
parseDecimal1 = do
  sign <- parseIntegerSign
  Number . (* sign) . read <$> many1 digit

parseDecimal2 :: Parser SchemeVal
parseDecimal2 = do
  try $ string "#d"
  sign <- parseIntegerSign
  x <- many1 digit
  (return . Number . (* sign) . read) x

parseHex :: Parser SchemeVal
parseHex = do
  try $ string "#x"
  sign <- parseIntegerSign
  x <- many1 hexDigit
  (return . Number . (* sign) . hex2dig) x

parseOct :: Parser SchemeVal
parseOct = do
  try $ string "#o"
  sign <- parseIntegerSign
  x <- many1 octDigit
  (return . Number . (* sign) . oct2dig) x

parseBin :: Parser SchemeVal
parseBin = do
  try $ string "#b"
  sign <- parseIntegerSign
  x <- many1 $ oneOf "01"
  (return . Number . (* sign) . bin2dig) x

oct2dig, hex2dig :: String -> Integer
oct2dig x = fst $ head (readOct x)
hex2dig x = fst $ head (readHex x)

bin2dig :: String -> Integer
bin2dig = bin2dig' 0

bin2dig' :: Integer -> String -> Integer
bin2dig' digint "" = digint
bin2dig' digint (x : xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in bin2dig' old xs

parseRatio :: Parser SchemeVal
parseRatio = do
  sign <- parseIntegerSign
  x <- many1 digit
  char '/'
  y <- many1 digit
  (return . Ratio) $ ((* sign) . read) x % read y

parseComplex :: Parser SchemeVal
parseComplex = do
  realSign <- parseDoubleSign
  realPart' <- try parseFloat <|> parseNumber
  imagSign <- parseDoubleSignStrict
  imagPart' <- try parseFloat <|> parseNumber
  char 'i'
  (return . Complex) $ ((* realSign) . toDouble) realPart' :+ ((* imagSign) . toDouble) imagPart'

toDouble :: SchemeVal -> Double
toDouble (Float f) = realToFrac f
toDouble (Number n) = fromIntegral n
toDouble _ = error "cannot convert to dobule"

parseBool :: Parser SchemeVal
parseBool = do
  char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseCharacter :: Parser SchemeVal
parseCharacter = do
  try $ string "#\\"
  value <- try (string "newline" <|> string "space") <|> do x <- anyChar; notFollowedBy alphaNum; return [x]
  return $
    Character $ case value of
      "space" -> ' '
      "newline" -> '\n'
      _ -> head value

parseList :: Parser SchemeVal
parseList = List <$> sepEndBy parseExpr (skipMany1 whitespace)

parseDottedList :: Parser SchemeVal
parseDottedList = do
  head' <- endBy parseExpr (skipMany1 whitespace)
  tail' <- char '.' >> skipMany whitespace >> parseExpr
  skipMany whitespace
  return $ DottedList head' tail'

parseParens :: Parser SchemeVal
parseParens = do
  char '('
  skipMany space
  x <- try parseDottedList <|> try parseList
  char ')'
  return x

parseQuoted :: Parser SchemeVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser SchemeVal
parseQuasiQuoted = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser SchemeVal
parseUnQuote = do
  char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseVector :: Parser SchemeVal
parseVector = do
  string "#("
  skipMany space
  vals <- sepEndBy parseExpr whitespace
  char ')'
  return $ Vector $ listArray (0, length vals - 1) vals

parseComment :: Parser ()
parseComment =
  void
    ( try (char ';')
        >> manyTill anyChar (void (char '\n') <|> eof)
    )
    <|> void
      ( try (string "#|")
          >> manyTill anyChar (void (string "|#"))
      )

whitespace :: Parser ()
whitespace = void (void (try (space <|> tab)) <|> try parseComment)
