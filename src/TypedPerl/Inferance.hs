{-# LANGUAGE FlexibleContexts #-}
module TypedPerl.Inferance (
  infer
  ) where
import TypedPerl.Inferance.Constraint
import TypedPerl.Inferance.TypeContext
import TypedPerl.Inferance.Unify
import TypedPerl.Substitute
import TypedPerl.Types
import Control.Monad.State
import Control.Monad.Error.Class
import TypedPerl.PerlAST
import qualified Data.Map as M

buildConstraint :: (MonadState TypeContext m, MonadError TypeError m) =>
                    PerlAST -> m (PerlType, Constraint)
buildConstraint ast = foldAST constrMapper ast

buildRecordConstraint :: (Ord k
                          , MonadState TypeContext m
                          , MonadError TypeError m) =>
                         m (PerlType, Constraint) -> k
                         -> (PerlRecs k -> PerlRecs k -> ConstraintItem)
                         -> (PerlRecs k -> PerlType)
                         -> m (PerlType, Constraint)
buildRecordConstraint mast k newconst newrectype = do
  (ty, c) <- mast
  newType <- freshType
  newRow1 <- freshRec
  newRow2 <- liftM (unionRec (M.fromList [(k, newType)])) freshRec
  return (newType, (newconst newRow1 newRow2):
                   (EqType ty (newrectype newRow1)):c)
  where
    unionRec m r = r {recMap = (M.union (recMap r) m)}

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
    implicitItem' ast n = buildRecordConstraint ast n EqArgs TypeArg
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
      newType <- freshType
      (ty, c) <- withContext ((VarSubImplicit, newType) :) mt
      return (TypeArrow newType ty, c)
    app' mt1 mts =  do
      (ty, c1) <- mt1
      (tys, c2) <- mts
      newType <- freshType
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

infer :: PerlAST -> Either TypeError PerlType
infer t = evalStateT inferMain initialTypeContext
  where
    inferMain = do
      (t', c) <- buildConstraint t
      s <- unify c
      return (subst s t')
