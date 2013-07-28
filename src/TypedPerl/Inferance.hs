{-# LANGUAGE FlexibleContexts #-}
module TypedPerl.Inferance (
  infer
  ) where
import Control.Monad.Error.Class
import Control.Monad.State.Class
import Data.Monoid
import TypedPerl.PerlAST
import TypedPerl.PerlType
import TypedPerl.Substitute
import TypedPerl.Types
import TypedPerl.Utils
import Control.Monad.State
import qualified Data.Map as M
import Debug.Trace

type Constraint = [ConstraintItem]
data ConstraintItem =
  EqType PerlType PerlType
  | EqArgs (PerlRecs Int) (PerlRecs Int)
  | EqRecs (PerlRecs String) (PerlRecs String)
type Context = [(PerlVars, PerlType)]
data TypeContext = TypeContext {
  names :: TypeNames
  , context :: Context
  }
type TypeNames = [String]

freshName :: MonadState TypeContext m => m String
freshName = do
  name <- gets (head . names)
  modify (\s -> s {names = (tail . names) s})
  return name

typeNames :: TypeNames
typeNames = map (('a' :) . show) [(1 :: Integer)..]

buildConstraint :: PerlAST -> (PerlType, Constraint, TypeContext)
buildConstraint t = (ty, cns, ctx)
  where Right ((ty, cns), ctx) = -- TODO: should catch error
          runStateT (buildConstraint' t)
                    (TypeContext {names = typeNames, context = []})

buildConstraint' :: (MonadState TypeContext m, MonadError TypeError m) =>
                    PerlAST -> m (PerlType, Constraint)
buildConstraint' ast = foldAST constrMapper ast

buildRecordConstraint :: (Ord k
                          , MonadState TypeContext m
                          , MonadError TypeError m) =>
                         m (PerlType, Constraint) -> k
                         -> (PerlRecs k -> PerlRecs k -> ConstraintItem)
                         -> (PerlRecs k -> PerlType)
                         -> m (PerlType, Constraint)
buildRecordConstraint mast k newconst newrectype = do
  (ty, c) <- mast
  name <- freshName
  let newType = TypeVar (TypeNamed name)
  nameRow1 <- freshName
  let newRow1 = RecNamed nameRow1 M.empty
  nameRow2 <- freshName
  let newRow2 = RecNamed nameRow2 (M.fromList [(k, newType)])
  return (newType, (newconst newRow1 newRow2):
                   (EqType ty (newrectype newRow1)):c)

constrMapper :: (MonadState TypeContext m, MonadError TypeError m) =>
                PerlASTMapper (m (PerlType, Constraint))
                              (m (M.Map String PerlType, Constraint))
                              (m ([PerlType], Constraint))
constrMapper = PerlASTMapper {
  subDeclare = subDeclare'
  , declare = declare'
  , int = (const . return) (TypeBuiltin TypeInt, [])
  , str = (const . return) (TypeBuiltin TypeStr, [])
  , TypedPerl.PerlAST.var = var'
  , implicitItem = implicitItem'
  , op = op'
  , TypedPerl.PerlAST.obj = obj'
  , objMapItem = objMapItem'
  , objMapNil = objMapNil'
  , objItem = objItem'
  , abstract = abstract'
  , app = app'
  , appListCons = appListCons'
  , appListNil = appListNil'
  , TypedPerl.PerlAST.seq = seq'
  }
  where
    subDeclare' v mt =  do
      (ty', cns) <- mt
      modify (\tc -> tc {context = (v, ty'):context tc})
      return (TypeUnknown, cns)
    declare' v mt = do
      (ty', cns) <- mt
      modify (\tc -> tc {context = (v, ty'):context tc})
      return (ty', cns)
    var' v =  do
      ctx <- gets context
      case lookup v ctx of
        Just ty' -> return (ty', [])
        _ -> throwError ("Undefined variable " ++ show v)
    implicitItem' n = do
      let mimp = buildConstraint' (PerlVar VarSubImplicit)
      buildRecordConstraint mimp n EqArgs TypeArg
    op' o mt1 mt2 = do
      (ty1, c1) <- mt1
      (ty2, c2) <- mt2
      return (returnType o,
              (EqType ty1 $ leftType o):(EqType ty2 $ rightType o):c1 ++ c2)
    obj' mm _ = do
      (reco, c) <- mm
      return (TypeObj (RecEmpty reco), c)
    objMapItem' f mast mrec = do
      (ty, c) <- mast
      (reco, c') <- mrec
      return (M.insert f ty reco, c ++ c')
    objMapNil' = return (M.empty, [])
    objItem' mo f = buildRecordConstraint mo f EqRecs TypeObj
    abstract' mt = do
      name <- freshName
      let newType = TypeVar (TypeNamed name)
      ctx <- gets context
      modify (\tc -> tc {context = (VarSubImplicit, newType):ctx})
      (ty, c) <- mt
      modify (\tc -> tc {context = ctx}) -- restore ctx
      return (TypeArrow newType ty, c)
    app' mt1 mts =  do
      name <- freshName
      (ty, c1) <- mt1
      (tys, c2) <- mts
      let newType = TypeVar . TypeNamed $ name
      let argRec = RecEmpty (M.fromList (zip [0..] tys))
      let c = EqType ty (TypeArrow (TypeArg argRec) newType)
      return (newType, c : c2 ++ c1)
    appListCons' mast mapp = do
      (ty, c)<- mast
      (tys, c') <- mapp
      return (ty:tys, c ++ c')
    appListNil' = return ([], [])
    seq' mt1 mt2 = do
      (_, c1) <- mt1
      (ty, c2) <- mt2
      return (ty, c2 ++ c1)

type TypeError = String

unify :: (MonadState TypeContext m, MonadError TypeError m) =>
         Constraint -> m Substitute
unify [] = return []
unify ((EqType type1 type2):cs) = case (type1, type2) of
  (TypeUnknown, _) -> throwError "not defined"
  (t1, t2@TypeUnknown) -> unify ((EqType t2 t1):cs)
  (t1, t2) | t1 == t2 -> unify cs
  (TypeVar v, b@(TypeBuiltin _)) -> do
    ss <- unify (substC [SubstType v b] cs)
    return ((SubstType v b) : ss)
  (TypeVar v, t)
    | not $ t `elemTypeType` v ->
      do let s = SubstType v t
         ss <- unify (substC (s:[]) cs)
         return (s:ss)
  (t1, t2@(TypeVar _)) -> -- t1 mustn't be TypeVar (See above guard sentences)
    unify ((EqType t2 t1):cs)
  (TypeArrow t1 t1', TypeArrow t2 t2') ->
    unify ((EqType t1 t2):(EqType t1' t2'):cs)
  (TypeArg arg1, TypeArg arg2) -> unify ((EqArgs arg1 arg2):cs)
  (TypeObj obj1, TypeObj obj2) -> unify ((EqRecs obj1 obj2):cs)
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
  isntRecursive a1 a2 >> case (a1, a2) of
  (RecEmpty m, RecEmpty m')
    | M.null lackM && M.null lackM' -> unify (newConstraints ++ cs)
    | otherwise -> throwError ("Don't match rows of const arguments:"
                               ++ show m ++ "," ++ show m')
    where
      (newConstraints, lackM, lackM') = typesToConstr m m'
  (RecNamed s m, RecEmpty m')
    | M.null lackM' ->
       let substs = [newsubst s (RecEmpty lackM)]
           constr = newConstraints ++ cs
           constr' = substC substs constr
       in do substs' <- unify constr'
             return (substs ++ substs')
    | otherwise -> throwError (
                     "Oops, " ++ show m' ++ " has other keys:" ++ show m)
    where
      (newConstraints, lackM, lackM') = typesToConstr m m'
  (RecEmpty _, RecNamed _ _) -> unify ((newconst a2 a1):cs)
  (RecNamed s m, RecNamed s' m')
    -- Should I check if m or m' is empty?
    | s == s' -> unify (newconst (RecEmpty m) (RecEmpty m'):cs)
    | otherwise -> do
      newName <- freshName
      let substs = [
            newsubst s (RecNamed newName lackM)
            , newsubst s' (RecNamed newName lackM')
            ]
      let constr = newConstraints ++ cs
      let constr' = substC substs constr
      substs' <- unify constr'
      return (substs ++ substs')
    where
      (newConstraints, lackM, lackM') = typesToConstr m m'

isntRecursive :: (MonadState TypeContext m, MonadError TypeError m) =>
                 PerlRecs k -> PerlRecs k -> m ()
isntRecursive a b = isntRecursive' a b >> isntRecursive' b a
  where
    isntRecursive' (RecEmpty _) _ = return ()
    isntRecursive' (RecNamed n _) (RecEmpty m) =
      if elemMapRecs m n then throwError ("recursive row variable " ++ n)
                         else return ()
    isntRecursive' (RecNamed n _) (RecNamed _ m) =
      if elemMapRecs m n then throwError ("recursive row variable " ++ n)
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
elemTypeType ty v = (getAny . foldType mapper) ty
  where
    mapper = monoidMapper {TypedPerl.PerlType.var = Any . (v ==)}

elemTypeArgs :: PerlType -> RecsVar -> Bool
elemTypeArgs ty v = (getAny . foldType mapper) ty
  where
    mapper = monoidMapper {
      intRecNamed = mappend . Any . (v ==)
      , strRecNamed = mappend . Any . (v ==)
      }

elemMapRecs :: M.Map k PerlType -> RecsVar -> Bool
elemMapRecs m x = or $ map (flip elemTypeArgs x) (M.elems m)

substC :: Substitute -> Constraint -> Constraint
substC ss constr = map substConst' constr
  where substConst' (EqType a b) = EqType (substType' a) (substType' b)
        substConst' (EqArgs a b) = EqArgs (substRecs' a) (substRecs' b)
        substConst' (EqRecs a b) = EqRecs (substRecsStr' a) (substRecsStr' b)
        substType' = subst ss
        substRecs' = subst ss
        substRecsStr' = subst ss

infer :: PerlAST -> Either TypeError PerlType
infer t = do
  let (t', c, ctx) = buildConstraint t
  s <- evalStateT (unify c) ctx
  return (subst s t')
