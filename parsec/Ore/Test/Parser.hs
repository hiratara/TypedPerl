module Ore.Test.Parser (
  tests
  ) where
import Test.HUnit
import Ore.Parsec
import Ore.Types

tests :: Test
tests = TestList [
  (TestCase $ do
      let Right t = parsePerl "1"
      assertEqual "int" t (PerlInt 1)
  )
  ]
