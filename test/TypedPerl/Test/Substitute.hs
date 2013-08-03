module TypedPerl.Test.Substitute (
  tests
  ) where
import qualified Data.Set as S
import Test.HUnit
import TypedPerl.Substitute
import TypedPerl.Types
import TypedPerl.Inferance.TypeContext

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
  , (TestCase $ do
      let cty = PerlForall (S.singleton . TypeNamed $ "a", S.empty)
                           (TypeArrow (TypeVar (TypeNamed "a"))
                                      (TypeArrow (TypeVar (TypeNamed "b"))
                                                 (TypeVar (TypeNamed "b"))))
      let expectedCty = PerlForall
                        (S.singleton . TypeNamed $ "a", S.empty)
                        (TypeArrow (TypeVar (TypeNamed "a"))
                                   (TypeArrow (TypeBuiltin TypeInt)
                                              (TypeBuiltin TypeInt)))
      let s = [SubstType (TypeNamed "a") (TypeBuiltin TypeInt)
               , SubstType (TypeNamed "b") (TypeBuiltin TypeInt)
              ]
      let cty' = subst s cty
      assertEqual "subst ctype" expectedCty cty'
  )
  ]
