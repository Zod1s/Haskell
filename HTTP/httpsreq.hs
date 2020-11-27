{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = httpLBS "https://en.wikipedia.org/wiki/Soweto" >>= L8.putStrLn . getResponseBody