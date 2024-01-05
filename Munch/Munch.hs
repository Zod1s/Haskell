{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Text.ParserCombinators.Munch
-- Description : Simple parser combinator library
-- Copyright   : (c) Automattic, Inc. 2018
-- License     : GPL-3
-- Maintainer  : Nathan Bloomfield, nathan.bloomfield@a8c.com
-- Stability   : experimental
-- Portability : POSIX
-- Site        : http://nbloomf.blog/munch/munch.html
--
-- Parser combinator library with support for indentation sensitivity.
module Text.ParserCombinators.Munch
  ( -- * Usage
    -- $overview
    Parser (),
    runParser,
    debugParser,
    parseIO,
    Stream (),
    toStream,
    Token (..),
    Pos (..),
    DidConsume (..),

    -- * Basic Parsers
    token,
    bof,
    eof,
    satisfies,
    anyToken,
    wouldFail,
    wouldSucceed,
    choice,
    manySepBy,
    someSepBy,

    -- ** @Char@
    char,
    anyChar,
    newline,
    spaces,
    decimalDigit,
    hexDigit,
    lowerLatin,
    upperLatin,
    string,

    -- ** @Word8@
    byte,
    anyByte,
    anyAsciiChar,
    bytes,
    unicodeChar,
    unicodeString,

    -- * Errors
    (<?>),
    Error (..),
    BasicError (..),
    Annotation (..),
    ParseError (),
    displayParseError,

    -- * Indentation
    readRef,
    adjustRef,
    localRef,
    consume,
    ignore,
    Indentation (..),
    indent,

    -- ** Simple Indentation
    wrtRef,
    wrtPos,
    Endpoint (..),
    Relation (..),
    Dimension (..),

    -- * Permutations
    permute,
    permuteSepBy,
    permuteIndent,
    permuteIndentSepBy,
    (<$$>),
    (<&&>),
    (<$?>),
    (<&?>),
    Perms (),
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Fail qualified as F
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Char (chr)
import Data.List (intersperse, unwords)
import Data.Semigroup
import Data.String
import Data.Word (Word8)
import Numeric (showHex)

data DidConsume t
  = Declined
  | Consumed (Pos t) (Pos t) -- from first to second
  deriving (Show)

data Pos t = Pos
  { line :: Integer,
    column :: Integer
  }
  deriving (Eq, Ord, Show) -- compared on line and then on column

instance Semigroup (DidConsume t) where
  Declined <> Declined = Declined
  Consumed a b <> Declined = Consumed a b
  Declined <> Consumed a b = Consumed a b
  Consumed a b <> Consumed c d = Consumed (min a c) (max b d)

instance Monoid (DidConsume t) where
  mempty = Declined
  mappend = (<>)

data Stream t = Stream (Pos t) [t]

isEmpty :: Stream t -> Bool
isEmpty (Stream _ xs) = null xs

pointer :: Stream t -> Maybe (Pos t)
pointer (Stream pos xs) = case xs of
  [] -> Nothing
  _ -> Just pos

-- parser :: (Token t) => Stream t
--                     -> (DidConsume, Either ParseError (a, Stream t))

-- t represent the tokens' type

class Token t where
  initPos :: Pos t
  updatePos :: t -> Pos t -> Pos t
  formatPos :: Pos t -> String

toStream :: (Token t) => [t] -> Stream t
toStream = Stream initPos

popToken :: (Token t) => Stream t -> Maybe (t, Pos t, Stream t)
popToken (Stream pos xs) = case xs of
  [] -> Nothing
  z : zs -> Just (z, pos, Stream (updatePos z pos) zs)

newtype Parser t a = Parser
  { theParser ::
      Pos t ->
      Stream t ->
      (DidConsume t, Either (ParseError t) (a, Stream t))
  }

debugParser ::
  (Token t) =>
  Parser t a ->
  Stream t ->
  (DidConsume t, Either (ParseError t) (a, Stream t))
debugParser (Parser q) = q initPos

runParser ::
  (Token t) =>
  Parser t a ->
  Stream t ->
  Either (ParseError t) a
runParser (Parser q) stream = case q initPos stream of
  (_, Left err) -> Left err
  (_, Right (a, rest)) ->
    if isEmpty rest
      then Right a
      else Left $ Simply $ IncompleteParse (pointer rest)

parseIO ::
  (Token t, Pretty t, Show a) =>
  Parser t a ->
  Stream t ->
  IO ()
parseIO (Parser q) stream = case q initPos stream of
  (_, Left err) -> do
    putStrLn "Parse Error"
    putStrLn $ pretty err
  (_, Right (a, rest)) ->
    if isEmpty rest
      then do
        putStrLn "Parse OK"
        print a
      else do
        putStrLn "Parse Incomplete"
        print (pointer rest)

instance Functor (Parser t) where
  fmap f (Parser q) = Parser $ \ref stream ->
    let (c, result) = q ref stream
     in case result of
          Left err -> (c, Left err)
          Right (a, rest) -> (c, Right (f a, rest))

instance Applicative (Parser t) where
  pure x = Parser $ \_ stream ->
    (Declined, Right (x, stream))

  af <*> ax = do
    f <- af
    f <$> ax

instance Alternative (Parser t) where
  empty = Parser $ \_ _ ->
    (Declined, Left mempty)

  (Parser a) <|> (Parser b) = Parser $ \ref stream ->
    case a ref stream of
      (c, Right value) -> (c, Right value)
      (_, Left err1) -> case b ref stream of
        (c, Right value) -> (c, Right value)
        (c, Left err2) -> (c, Left $ err1 <> err2)

instance Monad (Parser t) where
  return = pure

  (Parser x) >>= f = Parser $ \ref stream ->
    let (c, result) = x ref stream
     in case c of
          Declined -> case result of
            Left err -> (Declined, Left err)
            Right (a, rest) -> theParser (f a) ref rest
          c1 ->
            let (c2, h) = case result of
                  Left err -> (Declined, Left err)
                  Right (a, rest) -> theParser (f a) ref rest
             in (c1 <> c2, h)

instance MonadPlus (Parser t)

instance F.MonadFail (Parser t) where
  fail msg = Parser $ \_ stream ->
    (Declined, Left $ Simply $ Failure msg (pointer stream))

instance (Semigroup a) => Semigroup (Parser t a) where
  (<>) = liftA2 (<>)

instance (Monoid a) => Monoid (Parser t a) where
  mempty = return mempty
  mappend = (<>)

token :: (Token t, Eq t) => t -> Parser t t
token c = Parser $ \_ stream ->
  case popToken stream of
    Nothing -> (Declined, Left $ Simply $ UnexpectedEOF (Right $ Just c))
    Just (a, pos, rest) ->
      if a == c
        then (Consumed pos pos, Right (a, rest))
        else (Declined, Left $ Simply $ UnexpectedToken a (Just c) pos)

eof :: (Token t) => Parser t ()
eof = Parser $ \_ stream ->
  case popToken stream of
    Nothing -> (Declined, Right ((), stream))
    Just (a, pos, _) ->
      (Declined, Left $ Simply $ UnexpectedToken a Nothing pos)

bof :: (Token t) => Parser t ()
bof = Parser $ \_ stream ->
  case popToken stream of
    Nothing -> (Declined, Left $ Simply $ UnexpectedEOF (Right Nothing))
    Just (_, pos, _) ->
      if pos == Pos 1 1
        then (Declined, Right ((), stream))
        else (Declined, Left $ Simply $ ExpectedBOF pos)

satisfies ::
  (Token t) =>
  (t -> Bool) ->
  String -> -- Human-readable name for the class of recognized tokens
  Parser t t
satisfies p msg = Parser $ \_ stream ->
  case popToken stream of
    Nothing -> (Declined, Left $ Simply $ UnexpectedEOF (Left msg))
    Just (a, pos, rest) ->
      if p a
        then (Consumed pos pos, Right (a, rest))
        else (Declined, Left $ Simply $ UnexpectedSatisfy a msg pos)

anyToken :: (Token t) => Parser t t
anyToken = satisfies (const True) "any token"

wouldFail :: (Token t) => Parser t a -> Parser t ()
wouldFail (Parser q) = Parser $ \ref stream ->
  let (_, r) = q ref stream
      h = case r of
        Left _ -> Right ((), stream)
        Right _ -> Left $ Simply $ UnexpectedSuccess (pointer stream)
   in (Declined, h)

wouldSucceed :: (Token t) => Parser t a -> Parser t ()
wouldSucceed (Parser q) = Parser $ \ref stream ->
  let (_, r) = q ref stream
      h = case r of
        Right _ -> Right ((), stream)
        Left e -> Left $ Because (Lookahead (pointer stream)) e
   in (Declined, h)

instance Token Char where
  updatePos c (Pos ln col) =
    if c == '\n'
      then Pos (ln + 1) 1
      else Pos ln (col + 1)

  initPos = Pos 1 1

  formatPos (Pos ln col) = concat ["1", show ln, "c", show col]

char :: Char -> Parser Char Char
char = token

anyChar :: Parser Char Char
anyChar = anyToken

newline :: Parser Char Char
newline = char '\n'

spaces :: Parser Char String
spaces = many $ char ' '

decimalDigit :: Parser Char Char
decimalDigit =
  satisfies
    (`elem` "0123456789")
    "decimal digit (0-9)"

hexDigit :: Parser Char Char
hexDigit =
  satisfies
    (`elem` "0123456789abcdefABCDEF")
    "hexadecimal digit (0-9, a-f, A-F)"

lowerLatin :: Parser Char Char
lowerLatin =
  satisfies
    (`elem` "abcdefghijklmnopqrstuvwxyz")
    "lower case latin letter (a-z)"

upperLatin :: Parser Char Char
upperLatin =
  satisfies
    (`elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    "upper case latin letter (A-Z)"

instance Token Word8 where
  updatePos _ (Pos _ offset) = Pos 0 (offset + 1)
  initPos = Pos 0 0
  formatPos (Pos _ offset) = unwords ["offset", "0x" ++ showHex offset ""]

byte :: Word8 -> Parser Word8 Word8
byte = token

anyByte :: Parser Word8 Word8
anyByte = anyToken

anyAsciiChar :: Parser Word8 Char
anyAsciiChar = chr . fromIntegral <$> anyByte

choice :: (Token t) => [Parser t a] -> Parser t a
choice = foldr (<|>) empty

someSepBy :: Parser t u -> Parser t a -> Parser t [a]
someSepBy sep p = (:) <$> p <*> many (sep >> p)

manySepBy :: Parser t u -> Parser t a -> Parser t [a]
manySepBy sep p = someSepBy sep p <|> return []

readRef :: (Token t) => Parser t (Pos t)
readRef = Parser $ \ref stream -> (Declined, Right (ref, stream))

adjustRef :: (Token t) => (Pos t -> Pos t) -> Parser t a -> Parser t a
adjustRef f (Parser p) = Parser $ \ref stream -> p (f ref) stream

localRef :: (Token t) => Pos t -> Parser t a -> Parser t a
localRef = adjustRef . const

consume :: (Token t) => Parser t a -> Parser t (a, (Pos t, Pos t))
consume (Parser p) = Parser $ \ref stream ->
  let (c, r) = p ref stream
   in case c of
        Declined ->
          let pos = pointer stream
           in case r of
                Right _ -> (Declined, Left $ Simply $ UnexpectedDecline pos)
                Left err -> (Declined, Left $ Because (DeclineReason pos) err)
        Consumed u v ->
          let h = case r of
                Left err -> Left err
                Right (a, rest) -> Right ((a, (u, v)), rest)
           in (Consumed u v, h)

ignore :: (Token t) => Parser t a -> Parser t ()
ignore (Parser q) = Parser $ \ref stream ->
  case q ref stream of
    (_, Left err) -> (Declined, Left err)
    (_, Right (_, rest)) -> (Declined, Right ((), rest))

data Indentation t = Indentation
  { relation :: Pos t -> (Pos t, Pos t) -> Bool,
    message :: Pos t -> (Pos t, Pos t) -> String
  }

indent ::
  (Token t) =>
  Indentation t ->
  Parser t a ->
  Parser t a
indent ind (Parser q) = Parser $ \ref stream ->
  let (c, r) = q ref stream
   in case c of
        Declined -> (c, r)
        Consumed u v ->
          case r of
            Left _ -> (c, r)
            Right _ ->
              if relation ind ref (u, v)
                then (c, r)
                else
                  ( Declined,
                    Left $
                      Simply $
                        UnexpectedIndentation
                          (message ind ref (u, v))
                          (u, v)
                  )

infix 0 <?>

(<?>) :: (Token t) => Parser t a -> String -> Parser t a
(Parser q) <?> msg = Parser $ \stack stream ->
  let (c, result) = q stack stream
   in case result of
        Right value -> (Declined, Right value)
        Left err -> (Declined, Left $ Because (Note msg (pointer stream)) err)

string :: String -> Parser Char String
string str = mapM char str <?> show str

bytes :: [Word8] -> Parser Word8 [Word8]
bytes bs = mapM byte bs <?> concatMap show bs

unicodeChar :: Char -> Parser Word8 [Word8]
unicodeChar c = mapM token (BS.unpack $ BSC.singleton c) <?> [c]

unicodeString :: String -> Parser Word8 [Word8]
unicodeString str = mconcat (map unicodeChar str) <?> str

data Error a e
  = OneOf [Error a e]
  | Because a (Error a e)
  | Simply e
  deriving (Eq, Show)

instance Semigroup (Error a e) where
  (OneOf es1) <> (OneOf es2) = OneOf (es1 ++ es2)
  (OneOf es1) <> y = case es1 of
    [] -> y
    _ -> OneOf $ es1 ++ [y]
  x <> (OneOf es2) = case es2 of
    [] -> x
    _ -> OneOf $ x : es2
  x <> y = OneOf [x, y]

instance Monoid (Error a e) where
  mempty = OneOf []
  mappend = (<>)

data BasicError t
  = UnexpectedEOF (Either String (Maybe t))
  | UnexpectedToken t (Maybe t) (Pos t)
  | UnexpectedSatisfy t String (Pos t)
  | UnexpectedIndentation String (Pos t, Pos t)
  | UnexpectedSuccess (Maybe (Pos t))
  | UnexpectedDecline (Maybe (Pos t))
  | ExpectedBOF (Pos t)
  | IncompleteParse (Maybe (Pos t))
  | Failure String (Maybe (Pos t))
  deriving (Eq, Show)

data Annotation t
  = Note String (Maybe (Pos t))
  | Lookahead (Maybe (Pos t))
  | DeclineReason (Maybe (Pos t))
  deriving (Eq, Show)

type ParseError t = Error (Annotation t) (BasicError t)

data Perms t a = Perms (Maybe a) [Branch t a]

data Branch t a = forall x. Branch (Perms t (x -> a)) (Parser t x)

perm :: a -> Perms t a
perm a = Perms (Just a) []

instance Functor (Perms t) where
  fmap f (Perms x xs) = Perms (fmap f x) (map (fmap f) xs)

instance Functor (Branch t) where
  fmap f (Branch t p) = Branch (fmap (f .) t) p

infixl 1 <&&>

(<&&>) :: Perms t (a -> b) -> Parser t a -> Perms t b
t@(Perms u bs) <&&> p = Perms Nothing $ Branch t p : map insert bs
  where
    insert (Branch w q) = Branch (fmap flip w <&&> p) q

infixl 2 <$$>

(<$$>) :: (a -> b) -> Parser t a -> Perms t b
f <$$> p = perm f <&&> p

infixl 1 <&?>

(<&?>) :: Perms t (a -> b) -> (a, Parser t a) -> Perms t b
t@(Perms u bs) <&?> (x, p) =
  Perms (fmap ($ x) u) $ Branch t p : map insert bs
  where
    insert (Branch w q) = Branch (fmap flip w <&?> (x, p)) q

infixl 2 <$?>

(<$?>) :: (a -> b) -> (a, Parser t a) -> Perms t b
f <$?> (x, p) = perm f <&?> (x, p)

permute :: (Token t) => Perms t a -> Parser t a
permute (Perms u bs) = choice $ map branch bs ++ nil
  where
    nil = case u of
      Nothing -> []
      Just x -> [return x]
    branch (Branch w p) = do
      x <- p
      f <- permute w
      return (f x)

permuteSepBy :: (Token t) => Parser t () -> Perms t a -> Parser t a
permuteSepBy = psep (pure ())
  where
    psep ::
      (Token t) =>
      Parser t () ->
      Parser t () ->
      Perms t a ->
      Parser t a
    psep init sep (Perms u bs) = choice $ map branch bs ++ nil
      where
        nil = case u of
          Nothing -> []
          Just x -> [return x]
        branch (Branch w p) = do
          init
          x <- p
          f <- psep sep sep w
          return (f x)

permuteIndent ::
  forall t a.
  (Token t) =>
  Indentation t ->
  Perms t a ->
  Parser t a
permuteIndent ind = pind
  where
    pind :: forall b. (Token t) => Perms t b -> Parser t b
    pind (Perms u bs) = choice $ map branch bs ++ nil
      where
        nil = case u of
          Nothing -> []
          Just x -> [return x]

        branch (Branch w p) = do
          (x, (u, _)) <- consume p
          f <- localRef u $ indent ind $ pind2 w
          return (f x)
    pind2 :: forall b. (Token t) => Perms t b -> Parser t b
    pind2 (Perms u bs) = choice $ map branch bs ++ nil
      where
        nil = case u of
          Nothing -> []
          Just x -> [return x]

        branch (Branch w p) = do
          x <- p
          f <- pind2 w
          return (f x)

permuteIndentSepBy ::
  forall t a.
  (Token t) =>
  Indentation t ->
  Parser t () ->
  Perms t a ->
  Parser t a
permuteIndentSepBy ind = pindsep (pure ())
  where
    pindsep ::
      forall b.
      (Token t) =>
      Parser t () ->
      Parser t () ->
      Perms t b ->
      Parser t b
    pindsep init sep (Perms u bs) = choice $ map branch bs ++ nil
      where
        nil = case u of
          Nothing -> []
          Just x -> [return x]

        branch (Branch w p) = do
          init
          (x, (u, _)) <- consume p
          f <- localRef u $ indent ind $ pindsep2 sep w
          return (f x)
    pindsep2 ::
      forall b.
      (Token t) =>
      Parser t () ->
      Perms t b ->
      Parser t b
    pindsep2 sep (Perms u bs) = choice $ map branch bs ++ nil
      where
        nil = case u of
          Nothing -> []
          Just x -> [return x]

        branch (Branch w p) = do
          sep
          x <- p
          f <- pindsep2 sep w
          return (f x)

class Pretty t where
  pretty :: t -> String

instance (Token t) => Pretty (Pos t) where
  pretty = formatPos

instance Pretty Char where
  pretty c = case c of
    '\n' -> "'\\n' (newline)"
    '\t' -> "'\\t' (tab)"
    '\v' -> "'\\v' (newline)"
    '\r' -> "'\\r' (newline)"
    _ -> ['\'', c, '\'']

instance (Token t, Pretty t) => Pretty (BasicError t) where
  pretty e = case e of
    UnexpectedEOF z -> case z of
      Left str -> unwords ["expected", str, "but reached end of stream"]
      Right c -> case c of
        Nothing ->
          "expected beginning of stream, but reached end of stream"
        Just w ->
          unwords
            ["expected", pretty w, "but reached end of stream"]
    UnexpectedToken c z pos -> case z of
      Nothing ->
        unwords
          ["expected EOF but read", pretty c, "at", pretty pos]
      Just d ->
        unwords
          ["expected", pretty d, "but read", pretty c, "at", pretty pos]
    UnexpectedSatisfy c name pos ->
      unwords
        ["expected", name, "but read", pretty c, "at", pretty pos]
    UnexpectedDecline pos -> case pos of
      Nothing ->
        "expected to consume characters but encountered EOF"
      Just u ->
        unwords
          ["expected to consume characters at", pretty u]
    UnexpectedIndentation msg (u1, u2) ->
      unwords ["expected", msg]
    ExpectedBOF pos ->
      unwords
        ["expected beginning of stream, but found position", pretty pos]
    IncompleteParse pos -> case pos of
      Nothing -> "expected to consume the entire stream"
      Just u ->
        unwords
          [ "expected to consume the entire stream",
            "but characters remain at position",
            pretty u
          ]
    Failure msg pos ->
      let loc = case pos of
            Nothing -> "end of stream:"
            Just u -> "at " ++ pretty u ++ ":"
       in unwords [loc, msg]

instance (Token t) => Pretty (Annotation t) where
  pretty a = case a of
    Note msg pos -> case pos of
      Just z -> unwords [msg, "at", pretty z]
      Nothing -> msg
    Lookahead pos -> case pos of
      Just z -> unwords ["successful lookahead at", pretty z]
      Nothing -> "successful lookahead at end of stream"
    DeclineReason pos -> case pos of
      Just z -> unwords ["consumption at", pretty z]
      Nothing -> "consume at EOF"

data Tree a = T a [Tree a]
  deriving (Show)

instance Functor Tree where
  fmap f (T x bs) = T (f x) (map (fmap f) bs)

renderTree :: Tree String -> String
renderTree = render . addPrefix
  where
    addPrefix :: Tree String -> Tree String
    addPrefix (T x bs) =
      T x $
        mapLast
          (mapRoot ("|- " ++) ("|  " ++))
          (mapRoot ("\'- " ++) ("   " ++))
          (map addPrefix bs)

    mapRoot :: (a -> b) -> (a -> b) -> Tree a -> Tree b
    mapRoot f g (T x bs) = T (f x) (map (fmap g) bs)

    mapLast :: (a -> b) -> (a -> b) -> [a] -> [b]
    mapLast f g as = case as of
      [] -> []
      [x] -> [g x]
      x : xs -> f x : mapLast f g xs

    render :: Tree String -> String
    render = concat . intersperse "\n" . flatten

    flatten :: Tree a -> [a]
    flatten (T x bs) = x : concatMap flatten bs

instance (Pretty a, Pretty e) => Pretty (Error a e) where
  pretty = renderTree . toTree
    where
      toTree :: (Pretty a, Pretty e) => Error a e -> Tree String
      toTree err = case err of
        OneOf es -> case es of
          [] -> T "unspecified failure :(" []
          _ -> T "one of the following:" $ map toTree es
        Because a err ->
          let msg = unwords ["expected", pretty a, "which failed due to"]
           in T msg [toTree err]
        Simply e -> T (pretty e) []

displayParseError :: (Token t, Pretty t) => ParseError t -> String
displayParseError = pretty

data Endpoint
  = Start
  | End
  deriving (Eq, Show)

data Dimension
  = Column
  | Line
  deriving (Eq, Show)

data Relation
  = Eq
  | Neq
  | Lt
  | Gt
  | Leq
  | Geq
  | Add Integer
  | Fix Integer
  deriving (Eq, Show)

wrtRef ::
  (Token t, Pretty t) =>
  Endpoint ->
  Dimension ->
  Relation ->
  Indentation t
wrtRef pt dim rel =
  Indentation
    { relation = \ref range ->
        getRel
          rel
          (getDim dim ref)
          ( getDim dim $
              getPt pt range
          ),
      message = \ref range ->
        let labelRef =
              if compareRef rel
                then
                  [ "that of the reference position at",
                    pretty ref
                  ]
                else []
         in unwords
              [ labelPt pt,
                labelDim dim,
                "of successful parse, at",
                pretty (getPt pt range) ++ ",",
                "to",
                labelRel rel
              ]
    }

wrtPos ::
  (Token t, Pretty t) =>
  Pos t ->
  Endpoint ->
  Dimension ->
  Relation ->
  Indentation t
wrtPos pos pt dim rel =
  Indentation
    { relation = \_ range ->
        getRel
          rel
          (getDim dim pos)
          ( getDim dim $
              getPt pt range
          ),
      message = \_ range ->
        let labelRef =
              if compareRef rel
                then
                  [ "that of position",
                    pretty pos
                  ]
                else []
         in unwords $
              [ labelPt pt,
                labelDim dim,
                "of successful parse, at",
                pretty (getPt pt range) ++ ",",
                "to",
                labelRel rel
              ]
                ++ labelRef
    }

getPt :: Endpoint -> (Pos t, Pos t) -> Pos t
getPt pt = case pt of
  Start -> fst
  End -> snd

getDim :: Dimension -> Pos t -> Integer
getDim dim = case dim of
  Column -> column
  Line -> line

getRel :: Relation -> Integer -> Integer -> Bool
getRel rel = case rel of
  Eq -> (==)
  Neq -> (/=)
  Lt -> (<)
  Gt -> (>)
  Leq -> (<=)
  Geq -> (>=)
  Add k -> \u v -> v == u + k
  Fix k -> \_ v -> v == k

labelPt :: Endpoint -> String
labelPt pt = case pt of
  Start -> "start"
  End -> "end"

labelDim :: Dimension -> String
labelDim dim = case dim of
  Column -> "column"
  Line -> "line"

labelRel :: Relation -> String
labelRel rel = case rel of
  Eq -> "equal"
  Neq -> "not equal"
  Lt -> "be less than"
  Leq -> "be less than or equal to"
  Gt -> "be greater than"
  Geq -> "be greater than or equal to"
  Add k -> case compare k 0 of
    GT -> "be exactly " ++ show k ++ " more than"
    EQ -> "equal"
    LT -> "be exactly " ++ show (-k) ++ " less than"
  Fix k -> "be exactly " ++ show k

compareRef :: Relation -> Bool
compareRef rel = case rel of
  Fix _ -> False
  _ -> True
