{-# LANGUAGE FlexibleContexts #-}
module TypedPerl.Inferance.Unify (
  unify
  , unifyUnsolvedConstr
  ) where
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.State.Class
import Data.Monoid
import TypedPerl.Inferance.Constraint
import TypedPerl.Inferance.TypeContext
import TypedPerl.PerlRecs
import TypedPerl.PerlType
import TypedPerl.Substitute
import TypedPerl.Types
import TypedPerl.Utils
import qualified Data.Map as M

unifyUnsolvedConstr  :: (MonadState TypeContext m, MonadError TypeError m) =>
              UnsolvedConstr -> m UnsolvedConstr
unifyUnsolvedConstr (c, s) = do
  s' <- unify (subst s c)
  return ([], (compSubst s' s))

unify :: (MonadState TypeContext m, MonadError TypeError m) =>
         Constraint -> m Substitute
unify [] = return (emptySubst)
unify ((EqType type1 type2):cs) = case (type1, type2) of
  (t1, t2) | t1 == t2 -> unify cs
  (TypeVar v, TypeUnknown) -> do
    let s = SubstType v TypeUnknown
    (`compSubst` s `addSubst` emptySubst) `liftM` unify (subst1 s cs)
  (TypeVar v, b@(TypeBuiltin _)) -> do
    ss <- unify (subst1 (SubstType v b) cs)
    return (ss `compSubst` SubstType v b `addSubst` emptySubst)
  (TypeVar v, t)
    | t `elemTypeType` v -> do
      v' <- TypeNamed `liftM` freshName
      let ty' = subst1 (SubstType v (TypeVar v')) t -- assign the fresh name
      let s = SubstType v (TypeFix v' ty')
      ss <- unify (subst1 s cs)
      let ss' = (ss `compSubst` s `addSubst` emptySubst)
      return ss'
    | otherwise ->
      do let s = SubstType v t
         ss <- unify (subst1 s cs)
         return (ss `compSubst` s `addSubst` emptySubst)
  (t1, t2@(TypeVar _)) -> -- t1 mustn't be TypeVar (See above guard sentences)
    unify ((EqType t2 t1):cs)
  (TypeArrow t1 t1', TypeArrow t2 t2') ->
    unify ((EqType t1 t2):(EqType t1' t2'):cs)
  (TypeFix v1 ty1, TypeFix v2 ty2)
    | v1 /= v2 -> do
      v' <- TypeNamed `liftM` freshName
      -- Normalize bound type variable
      let x1 = TypeFix v' (subst1 (SubstType v1 (TypeVar v')) ty1)
      let x2 = TypeFix v' (subst1 (SubstType v2 (TypeVar v')) ty2)
      unify (EqType x1 x2:cs)
  (TypeFix v (TypeVar v'), _)
    | v == v' -> error ("[BUG]Can't extract a recursive type" ++ show v)
  (ty1@(TypeFix v ty1'), ty2) -> do
    let ty1'' = subst1 (SubstType v ty1) ty1'
    unify ((EqType ty1'' ty2):cs)
  (ty1, ty2@(TypeFix _ _)) ->
    unify ((EqType ty2 ty1):cs)
  (TypeArg arg1, TypeArg arg2) -> unify ((EqArgs arg1 arg2):cs)
  (TypeObj fi1 me1, TypeObj fi2 me2) ->
    unify ((EqRecs me1 me2):(EqRecs fi1 fi2):cs)
  (t1, t2) -> throwError (
                "Couldn't find answer:" ++ show t1 ++ "==" ++ show t2)
unify ((EqArgs a1 a2):cs) = unifyRecs a1 a2 cs EqArgs SubstArgs
unify ((EqRecs a1 a2):cs) = unifyRecs a1 a2 cs EqRecs SubstRecs

unifyRecs :: (Show k, Ord k,
              MonadState TypeContext m, MonadError TypeError m) =>
             PerlRecs k -> PerlRecs k -> Constraint
             -> (PerlRecs k -> PerlRecs k -> ConstraintItem)
             -> (RecsVar -> PerlRecs k -> SubstituteItem)
             -> m Substitute
unifyRecs a1 a2 cs newconst newsubst =
  case (a1, a2) of
  (RecEmpty m, RecEmpty m')
    | M.null lackM && M.null lackM' -> unify (newConstraints ++ cs)
    | otherwise -> throwError ("Don't match rows of const arguments:"
                               ++ show m ++ "," ++ show m')
    where
      (newConstraints, lackM, lackM') = typesToConstr m m'
  (RecNamed s m, RecEmpty m')
    | M.null lackM' ->
       let substs = newsubst s (RecEmpty lackM)
           constr = newConstraints ++ cs
           constr' = subst1 substs constr
       in do substs' <- unify constr'
             return (substs' `compSubst` substs `addSubst` emptySubst)
    | otherwise -> throwError (
                     "Oops, " ++ show m' ++ " has other keys:" ++ show m)
    where
      (newConstraints, lackM, lackM') = typesToConstr m m'
  (RecEmpty _, RecNamed _ _) -> unify ((newconst a2 a1):cs)
  (RecNamed s m, RecNamed s' m')
    -- Should I check if m or m' is empty?
    | s == s' -> unify (newconst (RecEmpty m) (RecEmpty m'):cs)
    | otherwise -> do
      newRec <- freshRec
      let substs =
            newsubst s (unionRec lackM newRec)
            `addSubst` newsubst s' (unionRec lackM' newRec)
            `addSubst` emptySubst
      let constr = newConstraints ++ cs
      let constr' = subst substs constr
      substs' <- unify constr'
      return (substs' `compSubst` substs)
    where
      (newConstraints, lackM, lackM') = typesToConstr m m'

typesToConstr :: Ord k => M.Map k PerlType -> M.Map k PerlType ->
                 (Constraint, M.Map k PerlType, M.Map k PerlType)
typesToConstr m m' = (constraints, deleteKeys sames m', deleteKeys sames m)
  where
    constraints = map (\k -> EqType (unsafeLookup k m) (unsafeLookup k m'))
                      sames
    sames = sameKeys m m'
    unsafeLookup k m'' = let Just v = M.lookup k m'' in v

elemTypeType :: PerlType -> PerlTypeVars -> Bool
elemTypeType ty v = (getAny . foldType mapper) ty
  where
    mapper = monoidMapper {
      TypedPerl.PerlType.var = Any . (v ==)
      , fix = fix'
      }
    fix' v' ty' | v == v' = error ("[BUG]Duplicated " ++ show v')
                | otherwise = fix monoidMapper v' ty'
