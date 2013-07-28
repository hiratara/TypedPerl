{-# LANGUAGE FlexibleInstances #-}
module TypedPerl.Inferance.Constraint (
  ConstraintItem (..), Constraint, UnsolvedConstr
  ) where
import TypedPerl.Substitute
import TypedPerl.Types

type Constraint = [ConstraintItem]
data ConstraintItem =
  EqType PerlType PerlType
  | EqArgs (PerlRecs Int) (PerlRecs Int)
  | EqRecs (PerlRecs String) (PerlRecs String)

type UnsolvedConstr = (Constraint, Substitute)

instance Substable ConstraintItem where
  subst ss = substConst'
    where
      substConst' (EqType a b) = EqType (subst ss a) (subst ss b)
      substConst' (EqArgs a b) = EqArgs (subst ss a) (subst ss b)
      substConst' (EqRecs a b) = EqRecs (subst ss a) (subst ss b)
