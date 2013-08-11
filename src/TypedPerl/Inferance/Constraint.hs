{-# LANGUAGE FlexibleInstances #-}
module TypedPerl.Inferance.Constraint (
  ConstraintItem (..), Constraint, UnsolvedConstr
  , emptyConstr, addConstr
  , showConstraint
  ) where
import Data.List
import TypedPerl.Substitute
import TypedPerl.Types

type Constraint = [ConstraintItem]
data ConstraintItem =
  EqType PerlType PerlType
  | EqArgs (PerlRecs Int) (PerlRecs Int)
  | EqRecs (PerlRecs String) (PerlRecs String)
  deriving Show

type UnsolvedConstr = (Constraint, Substitute)

emptyConstr :: UnsolvedConstr
emptyConstr = ([], emptySubst)

infixr 6 `addConstr`
addConstr :: ConstraintItem -> UnsolvedConstr -> UnsolvedConstr
addConstr c (c', s) = (c:c', s)

instance Substitutable ConstraintItem where
  subst ss = substConst'
    where
      substConst' (EqType a b) = EqType (subst ss a) (subst ss b)
      substConst' (EqArgs a b) = EqArgs (subst ss a) (subst ss b)
      substConst' (EqRecs a b) = EqRecs (subst ss a) (subst ss b)

showConstraintItem :: ConstraintItem -> String
showConstraintItem (EqType a b) = showPerlType a ++ " == " ++ showPerlType b
showConstraintItem (EqArgs a b) = showPerlRecs a ++ " == " ++ showPerlRecs b
showConstraintItem (EqRecs a b) = showPerlRecs a ++ " == " ++ showPerlRecs b

showConstraint :: Constraint -> String
showConstraint cs = "#### CONSTRAINTS\n" ++ body ++ "\n####"
  where
    body = intercalate "\n" (map showConstraintItem cs)
