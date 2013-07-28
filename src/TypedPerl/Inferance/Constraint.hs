module TypedPerl.Inferance.Constraint (
  ConstraintItem (..), Constraint
  , substC
  ) where
import TypedPerl.Substitute
import TypedPerl.Types

type Constraint = [ConstraintItem]
data ConstraintItem =
  EqType PerlType PerlType
  | EqArgs (PerlRecs Int) (PerlRecs Int)
  | EqRecs (PerlRecs String) (PerlRecs String)

substC :: Substitute -> Constraint -> Constraint
substC ss constr = map substConst' constr
  where substConst' (EqType a b) = EqType (substType' a) (substType' b)
        substConst' (EqArgs a b) = EqArgs (substRecs' a) (substRecs' b)
        substConst' (EqRecs a b) = EqRecs (substRecsStr' a) (substRecsStr' b)
        substType' = subst ss
        substRecs' = subst ss
        substRecsStr' = subst ss
