module TypedPerl.Test.Substitute (
  tests
  ) where
import Test.HUnit
import TypedPerl.Substitute
import TypedPerl.Types

tests :: Test
tests = TestList [
  (TestCase $ do
      let ty = TypeVar (TypeNamed "a")
      let s = [  SubstType (TypeNamed "a") (TypeVar (TypeNamed "b"))
               , SubstType (TypeNamed "b") (TypeVar (TypeNamed "c"))]
      let ty' = subst s ty
      assertEqual "subst deeply" (TypeVar (TypeNamed "c")) ty'
  )
  , (TestCase $ do
      let ty = TypeVar (TypeNamed "a")
      let s = [  SubstType (TypeNamed "a")
                           (TypeArrow (TypeVar (TypeNamed "b"))
                                      (TypeVar (TypeNamed "b")))
               , SubstType (TypeNamed "b") (TypeBuiltin TypeInt)]
      let ty' = subst s ty
      assertEqual "subst deeply"
                  (TypeArrow (TypeBuiltin TypeInt)
                             (TypeBuiltin TypeInt))
                  ty'
  )
  ]
