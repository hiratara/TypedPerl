module TypedPerl.Test.Substitute (
  tests
  ) where
import qualified Data.Map as M
import qualified Data.Set as S
import Test.HUnit
import TypedPerl.Substitute
import TypedPerl.Types
import TypedPerl.Inferance.TypeContext

tests :: Test
tests = TestList [
  (TestCase $ do
      let ty = TypeVar (TypeNamed "a")
      let s = SubstType (TypeNamed "b") (TypeVar (TypeNamed "c"))
              `addSubst` SubstType (TypeNamed "a") (TypeVar (TypeNamed "b"))
              `addSubst` emptySubst
      let ty' = subst s ty
      assertEqual "subst deeply" (TypeVar (TypeNamed "c")) ty'
  )
  , (TestCase $ do
      let ty = TypeVar (TypeNamed "a")
      let s = SubstType (TypeNamed "b") (TypeBuiltin TypeInt)
              `addSubst` SubstType (TypeNamed "a")
                                   (TypeArrow (TypeVar (TypeNamed "b"))
                                              (TypeVar (TypeNamed "b")))
              `addSubst` emptySubst
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
      let s = SubstType (TypeNamed "a") (TypeBuiltin TypeInt)
              `addSubst` SubstType (TypeNamed "b") (TypeBuiltin TypeInt)
              `addSubst` emptySubst
      let cty' = subst s cty
      assertEqual "subst ctype" expectedCty cty'
  )
  , (TestCase $ do
      let s' = SubstType (TypeNamed "a")
                  (TypeArg (RecNamed "b"
                            (M.singleton 0 (TypeVar (TypeNamed "c")))))
      let expectedS' = SubstType (TypeNamed "a")
                  (TypeArg (RecEmpty
                            (M.singleton 0 (TypeBuiltin TypeInt))))
      let s = SubstArgs "b" (RecEmpty M.empty)
              `addSubst` SubstType (TypeNamed "c") (TypeBuiltin TypeInt)
              `addSubst` emptySubst
      let s'' = subst s s'
      assertEqual "subst ctype" expectedS' s''
  )
  ]
