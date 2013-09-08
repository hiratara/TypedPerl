module TypedPerl.Test.Parser (
  tests
  ) where
import Test.HUnit
import qualified Data.Map as M
import TypedPerl.Parsec
import TypedPerl.Types

asAST :: PerlAST' -> PerlAST
asAST ast' = PerlAST (PerlInfo "" 0 0) ast'

tPerlDeclare = (asAST .) . PerlDeclare
tPerlInt = asAST . PerlInt
tPerlStr = asAST . PerlStr
tPerlVar = asAST . PerlVar
tPerlImplicitItem = (asAST .) . PerlImplicitItem
tPerlOp = ((asAST .) .) . PerlOp
tPerlObj = (asAST .) . PerlObj
tPerlObjItem = (asAST .) . PerlObjItem
tPerlObjMeth = ((asAST .) .) . PerlObjMeth
tPerlAbstract = asAST . PerlAbstract
tPerlApp = (asAST .) . PerlApp
tPerlSeq = (asAST .) . PerlSeq
tPerlPackage = (asAST .) . PerlPackage

tests :: Test
tests = TestList [
  (TestCase $ do
      let Right t = parsePerl "1"
      assertEqual "int" (tPerlInt 1) t
  )
  , (TestCase $ do
      let Right t = parsePerl "x(1)"
      assertEqual "call" (tPerlApp (tPerlVar (VarSub "x")) [tPerlInt 1]) t
  )
  , (TestCase $ do
      let Right t = parsePerl "x()"
      assertEqual "call no args" (tPerlApp (tPerlVar (VarSub "x")) []) t
  )
  , (TestCase $ do
      let Right t = parsePerl "x(2, 3)"
      assertEqual "call 2 args"
                  (tPerlApp (tPerlVar (VarSub "x")) (map tPerlInt [2, 3])) t
  )
  , (TestCase $ do
      let Right t = parsePerl "$x->(2, 3)"
      assertEqual "call 2 args"
                  (tPerlApp (tPerlVar (VarNamed "x")) (map tPerlInt [2, 3])) t
  )
  , (TestCase $ do
      let Right t = parsePerl "bless {abc => 3}, \"MyClass\";"
      assertEqual "perl object"
                  (tPerlObj (M.fromList [("abc", tPerlInt 3)]) "MyClass") t
  )
  , (TestCase $ do
      let Right t = parsePerl "sub { $_[0]->{def} }"
      assertEqual "Field access"
                  (tPerlAbstract (tPerlObjItem
                    (tPerlImplicitItem (tPerlVar VarSubImplicit) 0) "def"))
                  t
  )
  , (TestCase $ do
      let Right t = parsePerl "1; package Hoge; 1; package Foo; 2;"
      assertEqual "package"
                  (tPerlSeq
                    (tPerlInt 1)
                  (tPerlSeq
                    (tPerlPackage "Hoge" (tPerlInt 1))
                    (tPerlPackage "Foo"  (tPerlInt 2))
                  ))
                  t
  )
  , (TestCase $ do
      let Right t = parsePerl "package Hoge::Foo; 1; package main; 2; 3;"
      assertEqual "package"
                  (tPerlSeq
                    (tPerlPackage "Hoge::Foo" (tPerlInt 1))
                    (tPerlPackage "main" (tPerlSeq (tPerlInt 2) (tPerlInt 3)))
                  )
                  t
  )
  ]
