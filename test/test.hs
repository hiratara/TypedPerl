module Main (main) where
import Test.HUnit
import qualified TypedPerl.Test.Parser as Parser
import qualified TypedPerl.Test.Inferance as Inferance

main :: IO ()
main = runTestTT testList >> return ()
  where
    testList = TestList [
        Parser.tests
      , Inferance.tests
      ]
