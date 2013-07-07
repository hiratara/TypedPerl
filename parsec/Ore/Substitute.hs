module Ore.Substitute (
  Substitute, substType
) where
import Ore.Types
type Substitute = [(PerlTypeVars, PerlType)]

substType :: Substitute -> PerlType -> PerlType
substType [] ty = ty
substType subst@((tyV', ty'):ss) ty@(TypeVar tyV)
  | tyV == tyV' = substType subst ty'
  | otherwise   = substType ss ty
substType _ b@(TypeBuiltin _) = b
substType ss (TypeArrow ty1 ty2) =
  TypeArrow (substType ss ty1) (substType ss ty2)
