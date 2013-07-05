module Ore.Inferance (
  infer
  ) where
import Ore.Substitute
import Ore.Types
import Control.Monad.State
import Debug.Trace

type Constraint = [(PerlType, PerlType)]
type Context = [(PerlVars, PerlType)]
data TypeContext = TypeContext {
  names :: TypeNames
  }
type TypeNames = [String]

freshName :: State TypeContext String
freshName = do
  name <- gets (head . names)
  modify (\s -> s {names = (tail . names) s})
  return name

typeNames :: TypeNames
typeNames = map (('a' :) . show) [(1 :: Integer)..]

buildConstraint :: PerlAST -> (PerlType, Constraint)
buildConstraint t = (ty, cns)
  where (ty, cns, _) = evalState (buildConstraint' [] t)
                         (TypeContext {names = typeNames})

buildConstraint' :: Context -> PerlAST
                    -> State TypeContext (PerlType, Constraint, Context)
buildConstraint' ctx (PerlDeclare v ty t) = do
  ty' <- if ty == TypeVar TypeUnknown
             then freshName >>= return . TypeVar . TypeNamed
             else return ty
  (ty'', cns, ctx') <- buildConstraint' ctx t
  return (TypeUnit ,(ty', ty''):cns, ((v, ty'):ctx'))
buildConstraint' ctx (PerlInt _) = return (TypeInt, [], ctx)
buildConstraint' ctx (PerlVar v) = do
  case lookup v ctx of
    Just ty' -> return (ty', [], ctx)
    -- Should report as errors
    _ -> return (tyUnknown, [(tyUnknown,tyUnknown)], ctx)
  where
    tyUnknown = TypeVar TypeUnknown

buildConstraint' ctx (PerlOp _ t1 t2) = do
  (ty1, c1, ctx') <- buildConstraint' ctx t1
  (ty2, c2, ctx'') <- buildConstraint' ctx' t2
  return (TypeInt, (ty1, TypeInt) : (ty2, TypeInt) : c1 ++ c2, ctx'')
buildConstraint' ctx (PerlAbstract t) = do
  name <- freshName
  let newType = TypeVar (TypeNamed name)
  (ty, c, _) <- buildConstraint' ((VarSubImplicit, newType):ctx) t
  return (TypeArrow newType ty, c, ctx)
buildConstraint' ctx (PerlApp t1 t2) = do
    name <- freshName
    (ty1, c1, ctx') <- buildConstraint' ctx t1
    (ty2, c2, ctx'') <- buildConstraint' ctx' t2
    let newType = TypeVar . TypeNamed $ name
    let c = (ty1, TypeArrow ty2 newType)
    return (newType, c : c2 ++ c1, ctx'')
buildConstraint' ctx (PerlSeq t1 t2) = do
    (_, c1, ctx') <- buildConstraint' ctx t1
    (ty, c2, ctx'') <- buildConstraint' ctx' t2
    return (ty, c2 ++ c1, ctx'')

type TypeError = String

unify :: Constraint -> Either TypeError Substitute
unify [] = return []
unify ((t1, t2):cs)
  | isUnknown t1 || isUnknown t2 = Left "not defined"
  | t1 == t2 = unify cs
  | isTypeVar t1 && isIntVar t2 = do
    ss <- unify (substC [(t1var, TypeInt)] cs)
    return ((t1var, TypeInt) : ss)
  | isTypeVar t2 && isIntVar t1 = do
    ss <- unify (substC [(t2var, TypeInt)] cs)
    return ((t2var, TypeInt) : ss)
  | isTypeVar t1 && isUnitVar t2 = do
    ss <- unify (substC [(t1var, TypeUnit)] cs)
    return ((t1var, TypeUnit) : ss)
  | isTypeVar t2 && isUnitVar t1 = do
    ss <- unify (substC [(t2var, TypeUnit)] cs)
    return ((t2var, TypeUnit) : ss)
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
        isUnknown (TypeVar TypeUnknown) = True
        isUnknown _ = False
        isArrowType (TypeArrow _ _) = True
        isArrowType _ = False
        isIntVar TypeInt = True
        isIntVar _ = False
        isUnitVar TypeUnit = True
        isUnitVar _ = False
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
