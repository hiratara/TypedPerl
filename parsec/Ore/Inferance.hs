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
  , context :: Context
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
  where (ty, cns) = evalState (buildConstraint' t)
                         (TypeContext {names = typeNames, context = []})

buildConstraint' :: PerlAST -> State TypeContext (PerlType, Constraint)
buildConstraint' (PerlSubDeclare v t) = do
  (ty', cns) <- buildConstraint' t
  modify (\tc -> tc {context = (v, ty'):context tc})
  return (TypeVar TypeUnknown , cns)
buildConstraint' (PerlDeclare v t) = do
  (ty', cns) <- buildConstraint' t
  modify (\tc -> tc {context = (v, ty'):context tc})
  return (ty', cns)
buildConstraint' (PerlStr _) = return (TypeBuiltin TypeStr, [])
buildConstraint' (PerlInt _) = return (TypeBuiltin TypeInt, [])
buildConstraint' (PerlVar v) = do
  ctx <- gets context
  case lookup v ctx of
    Just ty' -> return (ty', [])
    -- Should report as errors
    _ -> return (tyUnknown, [(tyUnknown,tyUnknown)])
  where
    tyUnknown = TypeVar TypeUnknown

buildConstraint' (PerlOp op t1 t2) = do
  (ty1, c1) <- buildConstraint' t1
  (ty2, c2) <- buildConstraint' t2
  return (returnType op,
          (ty1, leftType op) : (ty2, rightType op) : c1 ++ c2)
buildConstraint' (PerlAbstract t) = do
  name <- freshName
  let newType = TypeVar (TypeNamed name)
  ctx <- gets context
  modify (\tc -> tc {context = (VarSubImplicit, newType):ctx})
  (ty, c) <- (buildConstraint' t)
  modify (\tc -> tc {context = ctx}) -- restore ctx
  return (TypeArrow newType ty, c)
buildConstraint' (PerlApp t1 t2) = do
    name <- freshName
    (ty1, c1) <- buildConstraint' t1
    (ty2, c2) <- buildConstraint' t2
    let newType = TypeVar . TypeNamed $ name
    let c = (ty1, TypeArrow ty2 newType)
    return (newType, c : c2 ++ c1)
buildConstraint' (PerlSeq t1 t2) = do
    (_, c1) <- buildConstraint' t1
    (ty, c2) <- buildConstraint' t2
    return (ty, c2 ++ c1)

type TypeError = String

unify :: Constraint -> Either TypeError Substitute
unify [] = return []
unify ((t1, t2):cs)
  | isUnknown t1 || isUnknown t2 = Left "not defined"
  | t1 == t2 = unify cs
  | isTypeVar t1 && isBuiltin t2 = do
    ss <- unify (substC [(t1var, t2)] cs)
    return ((t1var, t2) : ss)
  | isTypeVar t2 && isBuiltin t1 = do
    ss <- unify (substC [(t2var, t1)] cs)
    return ((t2var, t1) : ss)
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
        isBuiltin (TypeBuiltin _) = True
        isBuiltin _ = False
        typeVar (TypeVar t) = t
        typeVar t = error $ show t ++ " isn't type variables."
        arrow (TypeArrow ta tb) = (ta, tb)
        arrow t = error $ show t ++ " isn't type arrow types."
        containedBy :: PerlTypeVars -> PerlType -> Bool
        containedBy v (TypeVar t) = v == t
        containedBy v (TypeArrow t1' t2') =
          v `containedBy` t1' || v `containedBy` t2'
        containedBy _ (TypeBuiltin _) = False

substC :: Substitute -> Constraint -> Constraint
substC subst constr = map (delta substType') constr
  where delta f (a, b) = (f a, f b)
        substType' = substType subst

infer :: PerlAST -> Either TypeError PerlType
infer t = do
  let (t', c) = buildConstraint t
  s <- unify c
  return (substType s t')
