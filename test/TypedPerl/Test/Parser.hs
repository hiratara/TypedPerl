module TypedPerl.Test.Parser (
  tests
  ) where
import Test.HUnit
import qualified Data.Map as M
import TypedPerl.Parsec
import TypedPerl.Types

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
  , (TestCase $ do
      let Right t = parsePerl "bless {abc => 3}, \"MyClass\";"
      assertEqual "perl object" t
        (PerlObj (M.fromList [("abc", PerlInt 3)]) "MyClass")
  )
  , (TestCase $ do
      let Right t = parsePerl "sub { $_[0]->{def} }"
      assertEqual "Field access" t
        (PerlAbstract (PerlObjItem
                       (PerlImplicitItem (PerlVar VarSubImplicit) 0) "def"))
  )
  ]
