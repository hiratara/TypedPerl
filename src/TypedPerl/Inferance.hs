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
import Data.Monoid
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
  , int = (const . return) (TypeBuiltin TypeInt, emptyConstr)
  , str = (const . return) (TypeBuiltin TypeStr, emptyConstr)
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
      (ty, cns) <- mt
      cns' <- unifyUnsolvedConstr cns
      let ty' = subst (snd cns') ty
      modify (\tc -> tc {context = (v, ty'):context tc})
      return (TypeUnknown, cns')
    declare' v mt = do
      (ty, cns) <- mt
      cns' <- unifyUnsolvedConstr cns
      let ty' = subst (snd cns') ty
      modify (\tc -> tc {context = (v, ty'):context tc})
      return (ty', cns')
    var' v =  do
      ctx <- gets context
      case lookup v ctx of
        Just ty' -> return (ty', emptyConstr)
        _ -> throwError ("Undefined variable " ++ show v)
    implicitItem' ast n = buildRecordConstraint ast n EqArgs TypeArg
    op' o mt1 mt2 = do
      (ty1, cns1) <- mt1
      (ty2, cns2) <- mt2
      return (returnType o
              , (EqType ty1 $ leftType o)
                `addConstr` (EqType ty2 $ rightType o)
                `addConstr` cns1 <> cns2)
    obj' mm _ = do
      (reco, cns) <- mm
      return (TypeObj (RecEmpty reco), cns)
    objMapItem' f mast mrec = do
      (ty, cns) <- mast
      (reco, cns') <- mrec
      return (M.insert f ty reco, cns <> cns')
    objMapNil' = return (M.empty, emptyConstr)
    objItem' mo f = buildRecordConstraint mo f EqRecs TypeObj
    abstract' mt = do
      newType <- freshType
      (ty, cns) <- withContext ((VarSubImplicit, newType) :) mt
      return (TypeArrow newType ty, cns)
    app' mt1 mts =  do
      (ty, cns1) <- mt1
      (tys, cns2) <- mts
      newType <- freshType
      let argRec = RecEmpty (M.fromList (zip [0..] tys))
      let c = EqType ty (TypeArrow (TypeArg argRec) newType)
      return (newType, (c `addConstr` cns2 <> cns1))
    appListCons' mast mapp = do
      (ty, cns)<- mast
      (tys, cns') <- mapp
      return (ty:tys, cns <> cns')
    appListNil' = return ([], emptyConstr)
    seq' mt1 mt2 = do
      (_, cns1) <- mt1
      (ty, cns2) <- mt2
      return (ty, cns2 <> cns1)

infer :: PerlAST -> Either TypeError PerlType
infer t = evalStateT inferMain initialTypeContext
  where
    inferMain = do
      (t', cns) <- buildConstraint t
      cns' <- unifyUnsolvedConstr cns
      return (subst (snd cns') t')
