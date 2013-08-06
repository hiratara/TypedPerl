{-# LANGUAGE FlexibleContexts, TupleSections #-}
module TypedPerl.Inferance (
  infer
  ) where
import TypedPerl.Inferance.Constraint
import TypedPerl.Inferance.TypeContext
import TypedPerl.Inferance.Builtins
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
buildRecordConstraint m k f g = liftM change3to2
                                      (buildRecordConstraint3 m k f g)
  where change3to2 (_, a, b) = (a, b)

buildRecordConstraint3 :: (Ord k
                          , MonadState TypeContext m
                          , MonadError TypeError m) =>
                         m (PerlType, UnsolvedConstr) -> k
                         -> (PerlRecs k -> PerlRecs k -> ConstraintItem)
                         -> (PerlRecs k -> PerlType)
                         -> m (PerlType, PerlType, UnsolvedConstr)
buildRecordConstraint3 mast k newconst newrectype = do
  (ty, cns) <- mast
  newType <- freshType
  newRow1 <- freshRec
  newRow2 <- liftM (unionRec (M.fromList [(k, newType)])) freshRec
  return (ty, newType
          , newconst newRow1 newRow2
            `addConstr` EqType ty (newrectype newRow1)
            `addConstr` cns)

constrMapper :: (MonadState TypeContext m, MonadError TypeError m) =>
                PerlASTMapper (m (PerlType, UnsolvedConstr))
                              (m (M.Map String PerlType, UnsolvedConstr))
                              (m ([PerlType], UnsolvedConstr))
constrMapper = PerlASTMapper {
  declare = declare'
  , int = (const . return) (TypeBuiltin TypeInt, emptyConstr)
  , str = (const . return) (TypeBuiltin TypeStr, emptyConstr)
  , TypedPerl.PerlAST.var = var'
  , implicitItem = implicitItem'
  , op = op'
  , TypedPerl.PerlAST.obj = obj'
  , objMapItem = objMapItem'
  , objMapNil = objMapNil'
  , objItem = objItem'
  , objMeth = objMeth'
  , abstract = abstract'
  , app = app'
  , appListCons = appListCons'
  , appListNil = appListNil'
  , TypedPerl.PerlAST.seq = seq'
  , package = package'
  }
  where
    declare' v mt = do
      (ty, cns) <- mt
      cv <- varWithNamespace v
      (cTy, cns') <- buildTypeSchema (ty, cns)
      modify (\tc -> tc {context = (cv, cTy):context tc})
      case v of
        -- subroutine definition has no meaningful types
        (VarSub _) -> return (TypeUnknown, cns')
        _          -> liftM (, cns') (extractCType cTy)
    var' v =  do
      cty <- gets context >>= lookupContext v
      case cty of
        Just ty' -> liftM (, emptyConstr) (extractCType ty')
        _ -> throwError ("Undefined variable " ++ show v)
    implicitItem' ast n = buildRecordConstraint ast n EqArgs TypeArg
    op' o mt1 mt2 = do
      (ty1, cns1) <- mt1
      (ty2, cns2) <- mt2
      return (returnType o
              , (EqType ty1 $ leftType o)
                `addConstr` (EqType ty2 $ rightType o)
                `addConstr` cns1 <> cns2)
    obj' mm className = do
      (reco, cns) <- mm
      meths <- lookupMethods className
      return (TypeObj (RecEmpty reco) meths, cns)
    objMapItem' f mast mrec = do
      (ty, cns) <- mast
      (reco, cns') <- mrec
      return (M.insert f ty reco, cns <> cns')
    objMapNil' = return (M.empty, emptyConstr)
    objItem' mo f = do
      fields <- freshRec
      let objByField fi = TypeObj fi fields
      buildRecordConstraint mo f EqRecs objByField
    objMeth' mo me mts = do
      fields <- freshRec
      let objByMeth me' = TypeObj fields me'
      (ty1, ty2, cns1) <- buildRecordConstraint3 mo me EqRecs objByMeth
      (tys, cns2) <- mts

      -- Assign fresh type and raw variables to $self
      -- (TODO: This doesn't work well. See TODO tests.)
      ctx <- gets context
      ty1' <- extractCType (asCTypeSchema ctx ty1)

      newType <- freshType
      let argRec = RecEmpty (M.fromList (zip [0..] (ty1':tys))) -- Add $self
      let c = EqType ty2 (TypeArrow (TypeArg argRec) newType)
      return (newType, c `addConstr` cns2 <> cns1)
    abstract' mt = do
      newType <- freshType
      vImpli <- varWithNamespace VarSubImplicit
      (ty, cns) <- withContext ((vImpli, asCType newType) :) mt
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
    package' name mt = withPackage (const name) mt
    buildTypeSchema (ty, cns) = do
      cns' <- unifyUnsolvedConstr cns

      -- Update all types by substitution
      modify (\ctx -> ctx {context = subst (snd cns') (context ctx)})
      let ty' = subst (snd cns') ty

      liftM (, cns') (liftM (flip asCTypeSchema ty') (gets context))

lookupMethods :: (MonadState TypeContext m) =>
                  String -> m (PerlRecs String)
lookupMethods ns = do
  ctx <- gets context
  assoc <- (sequence . concatMap (byNamespace ns)) ctx
  return (RecEmpty (M.fromList assoc))
  where
    byNamespace n' (PerlCVar (NsGlobal n'') (VarSub methName), cty)
      | n' == n'' = [liftM (methName ,) (extractCType cty)]
    byNamespace _ _ = []

infer :: PerlAST -> Either TypeError PerlType
infer t = evalStateT inferMain initialTypeContext
  where
    inferMain = do
      (t', cns) <- buildConstraint t
      cns' <- unifyUnsolvedConstr cns
      return (subst (snd cns') t')
