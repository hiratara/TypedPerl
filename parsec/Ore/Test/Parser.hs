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
  , (TestCase $ do
      let Right t = parsePerl "x(1)"
      assertEqual "call" t
        (PerlApp (PerlVar (VarSub "x")) [PerlInt 1])
  )
  , (TestCase $ do
      let Right t = parsePerl "x()"
      assertEqual "call no args" t
        (PerlApp (PerlVar (VarSub "x")) [])
  )
  , (TestCase $ do
      let Right t = parsePerl "x(2, 3)"
      assertEqual "call 2 args" t
        (PerlApp (PerlVar (VarSub "x")) (map PerlInt [2, 3]))
  )
  , (TestCase $ do
      let Right t = parsePerl "$x->(2, 3)"
      assertEqual "call 2 args" t
        (PerlApp (PerlVar (VarNamed "x")) (map PerlInt [2, 3]))
  )
  ]
