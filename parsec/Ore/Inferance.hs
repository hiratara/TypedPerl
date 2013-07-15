module Ore.Inferance (
  infer
  ) where
import Data.Monoid
import Ore.PerlType
import Ore.Substitute
import Ore.Types
import Ore.Utils
import Control.Monad.State
import qualified Data.Map as M
import Debug.Trace

type Constraint = [ConstraintItem]
data ConstraintItem =
  EqType PerlType PerlType
  | EqRecs (PerlRecs Int) (PerlRecs Int)
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
  return (TypeUnknown, cns)
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
    _ -> return (TypeUnknown, [EqType TypeUnknown TypeUnknown])
buildConstraint' (PerlImplicitItem n) = do
  (ty, c) <- buildConstraint' (PerlVar VarSubImplicit)
  name <- freshName
  let newType = TypeVar (TypeNamed name)
  nameRow1 <- freshName
  let newRow1 = RecNamed nameRow1 M.empty
  nameRow2 <- freshName
  let newRow2 = RecNamed nameRow2 (M.fromList [(n, newType)])
  return (newType, (EqRecs newRow1 newRow2):
                   (EqType ty (TypeArg newRow1)):c)
buildConstraint' (PerlOp op t1 t2) = do
  (ty1, c1) <- buildConstraint' t1
  (ty2, c2) <- buildConstraint' t2
  return (returnType op,
          (EqType ty1 $ leftType op):(EqType ty2 $ rightType op):c1 ++ c2)
buildConstraint' (PerlAbstract t) = do
  name <- freshName
  let newType = TypeVar (TypeNamed name)
  ctx <- gets context
  modify (\tc -> tc {context = (VarSubImplicit, newType):ctx})
  (ty, c) <- (buildConstraint' t)
  modify (\tc -> tc {context = ctx}) -- restore ctx
  return (TypeArrow newType ty, c)
buildConstraint' (PerlApp t1 ts) = do
    name <- freshName
    (ty1, c1) <- buildConstraint' t1
    (ty2, c2) <- buildRecConstraint ts
    let newType = TypeVar . TypeNamed $ name
    let c = EqType ty1 (TypeArrow ty2 newType)
    return (newType, c : c2 ++ c1)
    where
      buildRecConstraint ts' = do
        answers <- mapM buildConstraint' ts'
        let argCols = M.fromList $ zip [0..] (map fst answers)
        let constraints = concat $ map snd answers
        return (TypeArg (RecEmpty argCols), constraints)
buildConstraint' (PerlSeq t1 t2) = do
    (_, c1) <- buildConstraint' t1
    (ty, c2) <- buildConstraint' t2
    return (ty, c2 ++ c1)

type TypeError = String

unify :: Constraint -> Either TypeError Substitute
unify [] = return []
unify ((EqType type1 type2):cs) = case (type1, type2) of
  (TypeUnknown, _) -> Left "not defined"
  (t1, t2@TypeUnknown) -> unify ((EqType t2 t1):cs)
  (t1, t2) | t1 == t2 -> unify cs
  (TypeVar v, b@(TypeBuiltin _)) -> do
    ss <- unify (substC [SubstType v b] cs)
    return ((SubstType v b) : ss)
  (TypeVar v, t)
    | not $ t `elemTypeType` v ->
      do let subst = SubstType v t
         ss <- unify (substC (subst:[]) cs)
         return (subst:ss)
  (t1, t2@(TypeVar _)) -> -- t1 mustn't be TypeVar (See above guard sentences)
    unify ((EqType t2 t1):cs)
  (TypeArrow t1 t1', TypeArrow t2 t2') ->
    unify ((EqType t1 t2):(EqType t1' t2'):cs)
  (TypeArg arg1, TypeArg arg2) -> unify ((EqRecs arg1 arg2):cs)
  (t1, t2) -> Left ("Couldn't find answer:" ++ show t1 ++ "==" ++ show t2)
unify (c@(EqRecs a1 a2):cs) = isntRecursive c >> case (a1, a2) of
  (RecEmpty m, RecEmpty m')
    | M.null lackM && M.null lackM' -> unify (newConstraints ++ cs)
    | otherwise -> Left ("Don't match rows of const arguments:"
                         ++ show m ++ "," ++ show m')
    where
      (newConstraints, lackM, lackM') = typesToConstr m m'
  (RecNamed s m, RecEmpty m')
    | M.null lackM' ->
       let substs = [SubstRecs s (RecEmpty lackM)]
           constr = newConstraints ++ cs
           constr' = substC substs constr
       in do substs' <- unify constr'
             return (substs ++ substs')
    | otherwise -> Left ("Oops, " ++ show m' ++ " has other keys:" ++ show m)
    where
      (newConstraints, lackM, lackM') = typesToConstr m m'
  (RecEmpty _, RecNamed _ _) -> unify ((EqRecs a2 a1):cs)
  (RecNamed s m, RecNamed s' m')
    -- Should I check if m or m' is empty?
    | s == s' -> unify (EqRecs (RecEmpty m) (RecEmpty m'):cs)
    | otherwise ->
      let newName = s ++ "'" -- TODO: It's not new name!
          substs = [
            SubstRecs s (RecNamed newName lackM)
            , SubstRecs s' (RecNamed newName lackM')
            ]
          constr = newConstraints ++ cs
          constr' = substC substs constr
      in do substs' <- unify constr'
            return (substs ++ substs')
    where
      (newConstraints, lackM, lackM') = typesToConstr m m'

isntRecursive :: ConstraintItem -> Either TypeError ()
isntRecursive (EqType _ _) = error "[BUG]Not Implemented"
isntRecursive (EqRecs a b) = isntRecursive' a b >> isntRecursive' b a
  where
    isntRecursive' (RecEmpty _) _ = return ()
    isntRecursive' (RecNamed n _) (RecEmpty m) =
      if elemMapRecs m n then Left ("recursive row variable " ++ n)
                         else return ()
    isntRecursive' (RecNamed n _) (RecNamed _ m) =
      if elemMapRecs m n then Left ("recursive row variable " ++ n)
                         else return ()

typesToConstr :: Ord k => M.Map k PerlType -> M.Map k PerlType ->
                 (Constraint, M.Map k PerlType, M.Map k PerlType)
typesToConstr m m' = (constraints, deleteKeys sames m', deleteKeys sames m)
  where
    constraints = map (\k -> EqType (unsafeLookup k m) (unsafeLookup k m'))
                      sames
    sames = sameKeys m m'
    unsafeLookup k m'' = let Just v = M.lookup k m'' in v

elemTypeType :: PerlType -> PerlTypeVars -> Bool
elemTypeType ty v = getAny $ varsFoldMapType (\v' -> Any (v == v'))
                                             (const (Any False)) ty

elemTypeArgs :: PerlType -> RecsVar -> Bool
elemTypeArgs ty x = getAny $ varsFoldMapType (const (Any False))
                                             (\(RecNamed x' _) -> Any (x == x'))
                                             ty
elemMapRecs :: M.Map k PerlType -> RecsVar -> Bool
elemMapRecs m x = or $ map (flip elemTypeArgs x) (M.elems m)

substC :: Substitute -> Constraint -> Constraint
substC subst constr = map substConst' constr
  where substConst' (EqType a b) = EqType (substType' a) (substType' b)
        substConst' (EqRecs a b) = EqRecs (substRecs' a) (substRecs' b)
        substType' = substType subst
        substRecs' = substRecs subst

infer :: PerlAST -> Either TypeError PerlType
infer t = do
  let (t', c) = buildConstraint t
  s <- unify c
  return (substType s t')
