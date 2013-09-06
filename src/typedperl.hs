module Main (main) where
import System.Environment (getArgs)
import System.Exit
import System.IO
import TypedPerl.Inferance
import TypedPerl.Inferance.TypeContext
import TypedPerl.Parsec
import TypedPerl.Types

die :: String -> IO a
die message = do
  hPutStrLn stderr message
  exitFailure

main :: IO ()
main = do
  path <- getArgs
  if null path then die "Specify the file name" else return ()
  source <- readFile . head $ path
  ast <- either (die . show) return (parsePerl source)
  (ty, ctx) <- either (die . show) return (inferTypeAndContext ast)
  (putStrLn . showContext . context) ctx
  putStrLn "----"
  putStrLn (showPerlType ty)
  return ()
