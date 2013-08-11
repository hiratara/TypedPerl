{-# LANGUAGE FlexibleContexts #-}
module TypedPerl.Inferance.TypeContext (
  TypeContext (..), TypeNames, Context
  , PerlCVar (..), varWithNamespace
  , PerlCType (..), asCType, asCTypeSchema, extractCType
  , TypeError
  , freshName, freshType, freshRec
  , withContext, withPackage
  , lookupContext
  , showContext
  ) where
import Control.Monad
import Control.Monad.State.Class
import qualified Data.Map as M
import Data.List
import Data.Monoid
import qualified Data.Set as S
import TypedPerl.Types
import TypedPerl.PerlType

type VarSet = (S.Set PerlTypeVars, S.Set RecsVar)

data PerlCVar = PerlCVar PerlNamespace PerlVars deriving (Show, Eq)
data PerlCType = PerlForall VarSet PerlType
                 deriving (Show, Eq)
type Context = [(PerlCVar, PerlCType)]

type TypeError = String

data TypeContext = TypeContext {
  names :: TypeNames
  , curPackage :: String
  , context :: Context
  }
type TypeNames = [String]

asCType :: PerlType -> PerlCType
asCType = PerlForall (S.empty, S.empty)

freshName :: MonadState TypeContext m => m String
freshName = do
  name <- gets (head . names)
  modify (\s -> s {names = (tail . names) s})
  return name

freshType :: MonadState TypeContext m => m PerlType
freshType = liftM (TypeVar . TypeNamed) freshName

freshRec :: MonadState TypeContext m => m (PerlRecs k)
freshRec = liftM (flip RecNamed M.empty) freshName

withContext :: MonadState TypeContext m =>
                   (Context -> Context) -> m a -> m a
withContext f mx = do
  curCtx <- gets context
  x <- modify (\tc -> tc {context = (f . context) tc}) >> mx
  modify (\tc -> tc {context = curCtx})
  return x

withPackage :: MonadState TypeContext m =>
                   (String -> String) -> m a -> m a
withPackage f mx = do
  origPackage <- gets curPackage
  x <- modify (\tc -> tc {curPackage = (f . curPackage) tc}) >> mx
  modify (\tc -> tc {curPackage = origPackage})
  return x

asCTypeSchema :: Context -> PerlType -> PerlCType
asCTypeSchema ctx ty = PerlForall vs ty
  where
    vs = freeVar ty `minusVarSet` freeVarInContext ctx

freeVar :: PerlType -> VarSet
freeVar ty = foldType mapper ty
  where
    mapper = monoidMapper {
      var = var'
      , fix = fix'
      , intRecNamed = intRecNamed'
      , strRecNamed = strRecNamed'
      }
    var' v = (S.singleton v, S.empty)
    fix' v vss = vss `minusVarSet` (S.singleton v, S.empty)
    intRecNamed' rv vss = (S.empty, S.singleton rv) <> vss
    strRecNamed' rv vss = (S.empty, S.singleton rv) <> vss

freeVarInCType :: PerlCType -> VarSet
freeVarInCType (PerlForall vs ty) = freeVar ty `minusVarSet` vs

freeVarInContext :: Context -> VarSet -- TODO: Simplify
freeVarInContext ctx = foldr (\x y -> y `mappend` (freeVarInCType . snd $ x)) mempty ctx

minusVarSet :: VarSet -> VarSet -> VarSet
minusVarSet (rvs1, vs1) (rvs2, vs2) = (rvs1 S.\\ rvs2, vs1 S.\\ vs2)

extractCType :: MonadState TypeContext m => PerlCType -> m PerlType
extractCType (PerlForall (vs, rvs) ty) = do
  vNames <- S.foldr (walk TypeNamed) (return M.empty) vs
  rvNames <- S.foldr (walk id) (return M.empty) rvs
  return (foldType (mapper vNames rvNames) ty)
  where
    walk typing v mm = do
      n <- freshName
      liftM (M.insert v (typing n)) mm
    mapper vNames rvNames = nopMapper {
      var = var' vNames
      , fix = fix' vNames
      , strRecNamed = strRecNamed' rvNames
      , intRecNamed = intRecNamed' rvNames
      }
    var' vNames v
      | v `S.member` vs = var nopMapper (vNames M.! v)
      | otherwise = var nopMapper v
    fix' _ v ty'
      | v `S.member` vs = error ("[BUG]variable named " ++ show v
                                 ++ " is duplicated.")
      | otherwise = fix nopMapper v ty'
    strRecNamed' rvNames v mmap
      | v `S.member` rvs = strRecNamed nopMapper (rvNames M.! v) mmap
      | otherwise = strRecNamed nopMapper v mmap
    intRecNamed' rvNames v mmap
      | v `S.member` rvs = intRecNamed nopMapper (rvNames M.! v) mmap
      | otherwise = intRecNamed nopMapper v mmap

varWithNamespace :: MonadState TypeContext m => PerlVars -> m PerlCVar
varWithNamespace v@(VarSub _) = do
  pk <- gets curPackage
  return (PerlCVar (NsGlobal pk) v)
varWithNamespace v = return (PerlCVar NsLexical v)

lookupContext :: MonadState TypeContext m =>
                 PerlVars -> Context -> m (Maybe PerlCType)
lookupContext v ctx = mplus `liftM` lookup1 `ap` lookup2
  where
    lookup1 = do
      cv <- varWithNamespace v
      return (lookup cv ctx)
    lookup2 = return (lookup (PerlCVar (NsGlobal "CORE") v) ctx)

showVarSet :: VarSet -> String
showVarSet (vs, rvs) = intercalate " " (vs' ++ rvs')
  where
    vs' = map showPerlTypeVars (S.toList vs)
    rvs' = S.toList rvs

nullVarSet :: VarSet -> Bool
nullVarSet (vs, rvs) = S.null vs && S.null rvs

showPerlCType :: PerlCType -> String
showPerlCType (PerlForall vs ty)
  | nullVarSet vs = showPerlType ty
  | otherwise     = "∀" ++ showVarSet vs ++ "." ++ showPerlType ty

showPerlCVar :: PerlCVar -> String
showPerlCVar (PerlCVar ns cv) = case ns of
  NsLexical -> showPerlVars cv
  NsGlobal s -> s ++ "::" ++ showPerlVars cv

showContext :: Context -> String
showContext ctxs = "#### CONTEXT INFO\n" ++ body ++ "\n####"
  where
    body = intercalate ", \n" (map show' ctxs)
    show' (cv, cty) = showPerlCVar cv ++ " := " ++ showPerlCType cty
