import Print


import System.Environment


main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne $ args
