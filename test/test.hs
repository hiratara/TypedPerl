module Main (main) where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified TypedPerl.Test.Parser as Parser
import qualified TypedPerl.Test.Inferance as Inferance

main :: IO ()
main = (defaultMain . hUnitTestToTests) testList
  where
    testList = TestList [
        Parser.tests
      , Inferance.tests
      ]
