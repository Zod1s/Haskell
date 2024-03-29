module HttpParser
  ( HttpRequest (..),
    Method (..),
    Headers,
    pRequest,
    pQuery,
    showRequest,
    parseRequest,
  )
where

-- \| TODO : show the error in the body

-- \| Rifare con Megaparsec
import Control.Applicative (liftA2)
import Control.Monad (liftM2, liftM4, mapM_, void)
import Data.Maybe (fromMaybe)
import Numeric (readHex)
import System.IO (Handle)
import Text.ParserCombinators.Parsec

type Headers = [(String, String)]

data Method
  = None
  | Error
  | Head
  | Get
  | Post
  deriving (Eq, Ord)

data HttpRequest = HttpRequest
  { reqMethod :: Method,
    reqURL :: String,
    reqHeaders :: Headers,
    reqBody :: Maybe String
  }
  deriving (Eq, Show)

instance Show Method where
  show None = "NONE"
  show Error = "ERROR"
  show Head = "HEAD"
  show Get = "GET"
  show Post = "POST"

showRequest :: HttpRequest -> String
showRequest (HttpRequest Head _ headers _) = "HTTP/1.1 200 OK " ++ showHeaders headers
showRequest (HttpRequest Get _ headers body) = "HTTP/1.1 200 OK " ++ showHeaders headers ++ showMaybe body
showRequest (HttpRequest Post _ headers body) = "HTTP/1.1 200 OK " ++ showHeaders headers ++ showMaybe body
showRequest (HttpRequest Error _ _ _) = "HTTP/1.1 400 Bad Request"

showHeaders :: Headers -> String
showHeaders ((h, c) : xs) = "\n" ++ h ++ (": " ++ c) ++ showHeaders xs
showHeaders [] = []

showMaybe :: (Show a) => Maybe a -> String
showMaybe Nothing = ""
showMaybe (Just b) = show b

pRequest :: CharParser () HttpRequest
pRequest = q "HEAD" Head (pure Nothing) <|> q "GET" Get (Just <$> many anyChar) <|> q "POST" Post (Just <$> many anyChar)
  where
    q name ctor = liftM4 HttpRequest req url pHeaders
      where
        req = ctor <$ string name <* char ' '
    url = optional (char '/') *> manyTill notEOL (try $ string " HTTP/1." <* oneOf "01") <* crlf

isLegalHeaderLen :: Int -> CharParser st () -- checks if len(header name + header content) <= maxLen
isLegalHeaderLen maxLen = do
  s <- getInput
  case parse (Control.Monad.void (count (maxLen + 1) notEOL)) "" s of
    Left _ -> return ()
    Right _ -> fail "413 Request Entity Too Large"

pHeaders :: CharParser st [(String, String)]
pHeaders = isLegalHeaderLen 4096 >> header `manyTill` crlf
  where
    header = liftA2 (,) fieldName (char ':' *> spaces *> contents)
    fieldName = (:) <$> letter <*> many fieldChar
    fieldChar = letter <|> digit <|> oneOf "-_"
    contents = liftA2 (++) (many1 notEOL <* crlf) (continuation <|> pure [])
    continuation = liftA2 (:) (' ' <$ many1 (oneOf " \t")) contents

crlf :: CharParser st ()
crlf = void (string "\r\n") <|> void newline

notEOL :: CharParser st Char
notEOL = noneOf "\r\n"

pQuery :: CharParser () [(String, Maybe String)]
pQuery = pPair `sepBy` char '&'

pPair :: CharParser () (String, Maybe String)
pPair = liftM2 (,) (many1 pChar) (optionMaybe (char '=' >> many pChar))

pChar :: CharParser () Char
pChar = oneOf urlBaseChars <|> (char '+' >> return ' ') <|> pHex

urlBaseChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "$-_.!*'(),"

pHex :: CharParser () Char
pHex = hexify <$> (char '%' *> hexDigit) <*> hexDigit
  where
    hexify a b = toEnum . fst . head . readHex $ [a, b]

noneRequest :: HttpRequest
noneRequest = HttpRequest None "" [("", "")] Nothing

errorRequest :: HttpRequest
errorRequest = HttpRequest Error "" [("", "")] Nothing

parseRequest :: String -> HttpRequest
parseRequest request = unwrapRequest $ case runParser pRequest () "" request of
  Left err -> Nothing
  Right par -> Just par

unwrapRequest :: Maybe HttpRequest -> HttpRequest
unwrapRequest = Data.Maybe.fromMaybe errorRequest

httpRequest :: String
httpRequest = "HEAD /www.wikipedia.org HTTP/1.1\na:ciao\nb:cioa\n\ncontent"

infiniteHttpRequest :: String
infiniteHttpRequest = "HEAD /www.wikipedia.org HTTP/1.1\na:" ++ ['a', 'a' ..] ++ "\n\ncontent"

{- HEAD request : "HEAD /www.wikipedia.org HTTP/1.1\na:ciao\nb:cioa\n\ncontent" =
HttpRequest {reqMethod = Head, reqURL = "www.wikipedia.org", reqHeaders = [("a", "ciao"), ("b", "cioa")], reqBody = Nothing} -}

{- GET request : "GET /www.wikipedia.org HTTP/1.1\na:ciao\nb:cioa\n\ncontent" =
HttpRequest {reqMethod = Get, reqURL = "www.wikipedia.org", reqHeaders = [("a", "ciao"), ("b", "cioa")], reqBody = Just "content"} -}

{- POST request : "POST /www.wikipedia.org HTTP/1.1\na:ciao\nb:cioa\n\ncontent" =
HttpRequest {reqMethod = Post, reqURL = "www.wikipedia.org", reqHeaders = [("a", "ciao"), ("b", "cioa")], reqBody = Just "content"} -}