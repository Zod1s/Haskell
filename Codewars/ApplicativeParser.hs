{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

-- https://www.codewars.com/kata/54f1fdb7f29358dd1f00015d/train/haskell

import Data.Bifunctor (second)
import Data.Char
import Data.List
import Language.Haskell.TH (Exp)
import Prelude hiding (fmap)

-- | An ambiguous parser.
newtype Parser a = P {unP :: String -> [(String, a)]}

-- | Change the result of a parser.
pmap :: (a -> b) -> Parser a -> Parser b
pmap f (P p) = P $ \input -> [(rest, f a) | (rest, a) <- p input]

-- | Operator version of 'pmap'.
(<#>) :: (a -> b) -> Parser a -> Parser b
(<#>) = pmap

-- | Parse a value and replace it.
(<#) :: a -> Parser b -> Parser a
(<#) = pmap . const

-- this makes Parser Functor

infixl 4 <#>

infixl 4 <#

item :: Parser Char
item = P $ \s ->
  case s of
    [] -> []
    (c : cs) -> [(cs, c)]

-- | Parse a character only when a predicate matches.
predP :: (Char -> Bool) -> Parser Char
predP p = item >>== \input -> if p input then inject input else emptyP

-- | Succeed only when parsing the given character.
charP :: Char -> Parser Char
charP c = predP (c ==)

-- | Inject a value into an identity parser.
inject :: a -> Parser a
inject x = P $ \input -> [(input, x)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind (P p) f = P $ \input -> concatMap (\(rest, a) -> unP (f a) rest) $ p input

(>>==) :: Parser a -> (a -> Parser b) -> Parser b
(>>==) = bind

-- inject and bind make it a monad

-- | Given a parser with a function value and another parser, parse the function
-- first and then the value, return a parser which applies the function to the
-- value.
(<@>) :: Parser (a -> b) -> Parser a -> Parser b
(P pf) <@> (P px) = P (\s -> [(s2, f a) | (s1, f) <- pf s, (s2, a) <- px s1])

(<@) :: Parser a -> Parser b -> Parser a
pa <@ pb = (const <#> pa) <@> pb

-- (P pa) <@ (P pb) = P $ \input -> do
--   (rest, a) <- pa input
--   (rest', _) <- pb rest
--   return (rest', a)

(@>) :: Parser a -> Parser b -> Parser b
pa @> pb = (id <# pa) <@> pb

-- (P pa) @> (P pb) = P $ \input -> do
--   (rest, _) <- pa input
--   (rest', b) <- pb rest
--   return (rest', b)

-- this makes Parser Applicative

infixl 4 <@

infixl 4 @>

infixl 4 <@>

-- | Parse a whole string.
stringP :: String -> Parser String
stringP "" = inject ""
stringP (x : xs) = (:) <#> charP x <@> stringP xs

-- | Construct a parser that never parses anything.
emptyP :: Parser a
emptyP = P $ const []

-- | Combine two parsers: When given an input, provide the results of both parser run on the input.
(<<>>) :: Parser a -> Parser a -> Parser a
(P p1) <<>> (P p2) = P $ \input -> p1 input ++ p2 input

option :: Parser a -> Parser a -> Parser a
option (P pa) (P pb) = P $ \input -> case pa input of
  [] -> pb input
  s -> s

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) = option

-- emptyP and <<>> make it alternative

infixl 3 <<>>

-- | Apply the parser zero or more times.
many :: Parser a -> Parser [a]
many p = many_p
  where
    many_p = some_p <<>> emptyP
    some_p = (:) <#> p <@> many_p

-- | Apply the parser one or more times.
some :: Parser a -> Parser [a]
some p = some_p
  where
    many_p = some_p <<>> emptyP
    some_p = (:) <#> p <@> many_p

-- | Apply a parser and return all ambiguous results, but only those where the input was fully consumed.
runParser :: Parser a -> String -> [a]
runParser p cs = map snd $ filter (\(rest, x) -> null rest) (unP p cs)

-- | Apply a parser and only return a result, if there was only one unambiguous result with output fully consumed.
runParserUnique :: Parser a -> String -> Maybe a
runParserUnique p cs = case runParser p cs of
  [x] -> Just x
  _ -> Nothing

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> inject a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = p >>== rest
  where
    rest a = (op >>== (\f -> p >>== (rest . f a))) <|> inject a

-- | Kinds of binary operators.
data BinOp = AddBO | MulBO deriving (Eq, Show)

-- | Some kind of arithmetic expression.
data Expr
  = ConstE Int
  | BinOpE BinOp Expr Expr
  | NegE Expr
  | ZeroE
  deriving (Eq, Show)

evalExpr :: Expr -> Int
evalExpr ZeroE = 0
evalExpr (NegE expr) = -(evalExpr expr)
evalExpr (BinOpE AddBO expr1 expr2) = evalExpr expr1 + evalExpr expr2
evalExpr (BinOpE MulBO expr1 expr2) = evalExpr expr1 * evalExpr expr2
evalExpr (ConstE x) = x

-- | Parse arithmetic expressions, with the following grammar:
--
-- expr         ::= const | binOpExpr | neg | zero
-- const        ::= int
-- binOpExpr    ::= '(' expr ' ' binOp ' ' expr ')'
-- binOp        ::= '+' | '*'
-- neg          ::= '-' expr
-- zero         ::= 'z'
-- int          ::= digit +
-- digit        ::= '0' | ... | '9'
whiteSpace :: Parser Char
whiteSpace = charP ' '

parens :: Parser a -> Parser a
parens p = stringP "(" >>== (\_ -> p >>== (\m -> stringP ")" >>== (\_ -> inject m)))

digit :: Parser Char
digit = predP isDigit

parseZ :: Parser Expr
parseZ = ZeroE <# stringP "z"

parseNumber :: Parser Expr
parseNumber = ConstE . read <#> some digit

parseNeg :: Parser Expr
parseNeg = NegE <#> (stringP "-" @> parseExpr)

parseBinOp :: Parser (Expr -> Expr -> Expr)
parseBinOp = charToBinOp <#> (stringP " + " `option` stringP " * ")
  where
    charToBinOp " + " = BinOpE AddBO
    charToBinOp " * " = BinOpE MulBO

parseBinExpr :: Parser Expr
parseBinExpr = stringP "(" @> parseExpr `chainl1` parseBinOp <@ stringP ")"

parseExpr :: Parser Expr
parseExpr = parseNumber <|> parseZ <|> parseBinExpr <|> parseNeg

main :: IO ()
main = putStrLn "Hello, world!"
