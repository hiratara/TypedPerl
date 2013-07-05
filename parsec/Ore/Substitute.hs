module Ore.Substitute (
  Substitute, substType, substAST
) where
import Ore.Types
type Substitute = [(PerlTypeVars, PerlType)]

substType :: Substitute -> PerlType -> PerlType
substType [] ty = ty
substType ((tyV', ty'):ss) ty@(TypeVar tyV)
  | tyV == tyV' = ty'
  | otherwise   = substType ss ty
substType _ TypeInt = TypeInt
substType ss (TypeArrow ty1 ty2) =
  TypeArrow (substType ss ty1) (substType ss ty2)

substAST :: Substitute -> PerlAST -> PerlAST
substAST _ n@(PerlInt _) = n
substAST s (PerlVar v ty) = PerlVar v (substType s ty)
substAST s (PerlOp op t1 t2) =
  PerlOp op (substAST s t1) (substAST s t2)
substAST s (PerlAbstract t) = PerlAbstract (substAST s t)
substAST s (PerlApp t1 t2) =
  PerlApp (substAST s t1) (substAST s t2)
