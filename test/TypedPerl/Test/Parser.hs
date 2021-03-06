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
      assertEqual "int" (PerlInt 1) t
  )
  , (TestCase $ do
      let Right t = parsePerl "x(1)"
      assertEqual "call" (PerlApp (PerlVar (VarSub "x")) [PerlInt 1]) t
  )
  , (TestCase $ do
      let Right t = parsePerl "x()"
      assertEqual "call no args" (PerlApp (PerlVar (VarSub "x")) []) t
  )
  , (TestCase $ do
      let Right t = parsePerl "x(2, 3)"
      assertEqual "call 2 args"
                  (PerlApp (PerlVar (VarSub "x")) (map PerlInt [2, 3])) t
  )
  , (TestCase $ do
      let Right t = parsePerl "$x->(2, 3)"
      assertEqual "call 2 args"
                  (PerlApp (PerlVar (VarNamed "x")) (map PerlInt [2, 3])) t
  )
  , (TestCase $ do
      let Right t = parsePerl "bless {abc => 3}, \"MyClass\";"
      assertEqual "perl object"
                  (PerlObj (M.fromList [("abc", PerlInt 3)]) "MyClass") t
  )
  , (TestCase $ do
      let Right t = parsePerl "sub { $_[0]->{def} }"
      assertEqual "Field access"
                  (PerlAbstract (PerlObjItem
                    (PerlImplicitItem (PerlVar VarSubImplicit) 0) "def"))
                  t
  )
  , (TestCase $ do
      let Right t = parsePerl "1; package Hoge; 1; package Foo; 2;"
      assertEqual "package"
                  (PerlSeq
                    (PerlInt 1)
                  (PerlSeq
                    (PerlPackage "Hoge" (PerlInt 1))
                    (PerlPackage "Foo"  (PerlInt 2))
                  ))
                  t
  )
  , (TestCase $ do
      let Right t = parsePerl "package Hoge::Foo; 1; package main; 2; 3;"
      assertEqual "package"
                  (PerlSeq
                    (PerlPackage "Hoge::Foo" (PerlInt 1))
                    (PerlPackage "main" (PerlSeq (PerlInt 2) (PerlInt 3)))
                  )
                  t
  )
  ]
