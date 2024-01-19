import Print (runOne, runRepl)
import System.Environment (getArgs)

-- rlwrap to add history

main :: IO ()
main = do
  args <- getArgs
  if null args then runRepl else runOne args