{-# LANGUAGE FlexibleContexts #-}
module TypedPerl.Inferance (
  infer
  ) where
import TypedPerl.Inferance.Constraint
import TypedPerl.Inferance.TypeContext
import TypedPerl.Inferance.Unify
import TypedPerl.PerlRecs
import TypedPerl.Substitute
import TypedPerl.Types
import Control.Monad.State
import Control.Monad.Error.Class
import TypedPerl.PerlAST
import qualified Data.Map as M

buildConstraint :: (MonadState TypeContext m, MonadError TypeError m) =>
                    PerlAST -> m (PerlType, UnsolvedConstr)
buildConstraint ast = foldAST constrMapper ast

buildRecordConstraint :: (Ord k
                          , MonadState TypeContext m
                          , MonadError TypeError m) =>
                         m (PerlType, UnsolvedConstr) -> k
                         -> (PerlRecs k -> PerlRecs k -> ConstraintItem)
                         -> (PerlRecs k -> PerlType)
                         -> m (PerlType, UnsolvedConstr)
buildRecordConstraint mast k newconst newrectype = do
  (ty, (c, s)) <- mast
  newType <- freshType
  newRow1 <- freshRec
  newRow2 <- liftM (unionRec (M.fromList [(k, newType)])) freshRec
  return (newType
          , ((newconst newRow1 newRow2):(EqType ty (newrectype newRow1)):c
             , s))

constrMapper :: (MonadState TypeContext m, MonadError TypeError m) =>
                PerlASTMapper (m (PerlType, UnsolvedConstr))
                              (m (M.Map String PerlType, UnsolvedConstr))
                              (m ([PerlType], UnsolvedConstr))
constrMapper = PerlASTMapper {
  subDeclare = subDeclare'
  , declare = declare'
  , int = (const . return) (TypeBuiltin TypeInt, ([], []))
  , str = (const . return) (TypeBuiltin TypeStr, ([], []))
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
      (ty, (cns, s)) <- mt
      s' <- unifyUnsolvedConstr (cns, s)
      let ty' = subst s' ty
      modify (\tc -> tc {context = (v, ty'):context tc})
      return (TypeUnknown, ([], s'))
    declare' v mt = do
      (ty, (cns, s)) <- mt
      s' <- unifyUnsolvedConstr (cns, s)
      let ty' = subst s' ty
      modify (\tc -> tc {context = (v, ty'):context tc})
      return (ty', ([], s'))
    var' v =  do
      ctx <- gets context
      case lookup v ctx of
        Just ty' -> return (ty', ([], []))
        _ -> throwError ("Undefined variable " ++ show v)
    implicitItem' ast n = buildRecordConstraint ast n EqArgs TypeArg
    op' o mt1 mt2 = do
      (ty1, (c1, s1)) <- mt1
      (ty2, (c2, s2)) <- mt2
      return (returnType o
              , ((EqType ty1 $ leftType o):(EqType ty2 $ rightType o):c1 ++ c2
                 , s1 ++ s2))
    obj' mm _ = do
      (reco, (c, s)) <- mm
      return (TypeObj (RecEmpty reco), (c, s))
    objMapItem' f mast mrec = do
      (ty, (c, s)) <- mast
      (reco, (c', s')) <- mrec
      return (M.insert f ty reco, (c ++ c', s ++ s'))
    objMapNil' = return (M.empty, ([], []))
    objItem' mo f = buildRecordConstraint mo f EqRecs TypeObj
    abstract' mt = do
      newType <- freshType
      (ty, (c, s)) <- withContext ((VarSubImplicit, newType) :) mt
      return (TypeArrow newType ty, (c, s))
    app' mt1 mts =  do
      (ty, (c1, s1)) <- mt1
      (tys, (c2, s2)) <- mts
      newType <- freshType
      let argRec = RecEmpty (M.fromList (zip [0..] tys))
      let c = EqType ty (TypeArrow (TypeArg argRec) newType)
      return (newType, (c : c2 ++ c1, s1 ++ s2))
    appListCons' mast mapp = do
      (ty, (c, s))<- mast
      (tys, (c', s')) <- mapp
      return (ty:tys, (c ++ c', s ++ s'))
    appListNil' = return ([], ([], []))
    seq' mt1 mt2 = do
      (_, (c1, s1)) <- mt1
      (ty, (c2, s2)) <- mt2
      return (ty, (c2 ++ c1, s2 ++ s1))

infer :: PerlAST -> Either TypeError PerlType
infer t = evalStateT inferMain initialTypeContext
  where
    inferMain = do
      (t', (c, s)) <- buildConstraint t
      s' <- unifyUnsolvedConstr (c, s)
      return (subst s' t')
