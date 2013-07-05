module Ore.Inferance (
  infer
  ) where
import Ore.Substitute
import Ore.Types
import Control.Monad.State
import Debug.Trace

type Constraint = [(PerlType, PerlType)]
type Context = [(PerlVars, PerlType)]
type TypeNames = [String]

typeNames :: TypeNames
typeNames = map (('a' :) . show) [(1 :: Integer)..]

buildConstraint :: PerlAST -> (PerlType, Constraint)
buildConstraint t = evalState (buildConstraint' [] t) typeNames

buildConstraint' :: Context -> PerlAST
                    -> State TypeNames (PerlType, Constraint)
buildConstraint' _ (PerlInt _) = return (TypeInt, [])
buildConstraint' ctx (PerlVar v ty)
  | ty == TypeVar TypeUnknown = do
    let (Just ty') = lookup v ctx
    return (ty', [])
  | otherwise = return (ty, [])
buildConstraint' ctx (PerlOp _ t1 t2) = do
  (ty1, c1) <- buildConstraint' ctx t1
  (ty2, c2) <- buildConstraint' ctx t2
  return (ty1, (ty1, ty2) : c1 ++ c2)
buildConstraint' ctx (PerlAbstract t) = do
  name <- gets head
  modify tail
  let newType = TypeVar (TypeNamed name)
  (ty, c) <- buildConstraint' ((VarSubImplicit, newType):ctx) t
  return (TypeArrow newType ty, c)

buildConstraint' ctx (PerlApp t1 t2) = do
    name <- gets head
    modify tail
    (ty1, c1) <- buildConstraint' ctx t1
    (ty2, c2) <- buildConstraint' ctx t2
    let newType = TypeVar . TypeNamed $ name
    let c = (ty1, TypeArrow ty2 newType)
    return (newType, c : c2 ++ c1)

type TypeError = String

unify :: Constraint -> Either TypeError Substitute
unify [] = return []
unify ((t1, t2):cs)
  | t1 == t2 = unify cs
  | isTypeVar t1 && isIntVar t2 = do
    ss <- unify (substC [(t1var, TypeInt)] cs)
    return ((t1var, TypeInt) : ss)
  | isTypeVar t2 && isIntVar t1 = do
    ss <- unify (substC [(t2var, TypeInt)] cs)
    return ((t2var, TypeInt) : ss)
  | isTypeVar t1 && not (typeVar t1 `containedBy` t2) = do
    ss <- unify (substC [(t1var, t2)] cs)
    return ((t1var, t2) : ss)
  | isTypeVar t2 && not (typeVar t2 `containedBy` t1) = do
    ss <- unify (substC [(t2var, t1)] cs)
    return ((t2var, t1) : ss)
  | isArrowType t1 && isArrowType t2 =
      unify ((t1from, t2from) : (t1to, t2to) : cs)
  | otherwise = Left "Couldn't find answer"
  where t1var = typeVar t1
        t2var = typeVar t2
        (t1from, t1to) = arrow t1
        (t2from, t2to) = arrow t2
        isTypeVar (TypeVar _) = True
        isTypeVar _ = False
        isArrowType (TypeArrow _ _) = True
        isArrowType _ = False
        isIntVar TypeInt = True
        isIntVar _ = False
        typeVar (TypeVar t) = t
        typeVar t = error $ show t ++ " isn't type variables."
        arrow (TypeArrow ta tb) = (ta, tb)
        arrow t = error $ show t ++ " isn't type arrow types."
        containedBy :: PerlTypeVars -> PerlType -> Bool
        containedBy v (TypeVar t) = v == t
        containedBy v (TypeArrow t1' t2') =
          v `containedBy` t1' || v `containedBy` t2'
        containedBy _ TypeInt = False

substC :: Substitute -> Constraint -> Constraint
substC subst constr = map (delta substType') constr
  where delta f (a, b) = (f a, f b)
        substType' = substType subst

infer :: PerlAST -> Either TypeError PerlType
infer t = do
  let (t', c) = buildConstraint t
  s <- unify c
  return (substType s t')
