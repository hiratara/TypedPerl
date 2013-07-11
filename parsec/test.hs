module Main (main) where
import Test.HUnit
import qualified Ore.Test.Parser as Parser
import qualified Ore.Test.Inferance as Inferance

main :: IO ()
main = runTestTT testList >> return ()
  where
    testList = TestList [
        Parser.tests
      , Inferance.tests
      ]
