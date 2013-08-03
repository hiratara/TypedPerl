{-# LANGUAGE FlexibleContexts #-}
module TypedPerl.Inferance.TypeContext (
  TypeContext (..), TypeNames, Context
  , PerlCType (..), asCType, asCTypeSchema, extractCType
  , TypeError
  , freshType, freshRec
  , withContext
  ) where
import Control.Monad
import Control.Monad.State.Class
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S
import TypedPerl.Types
import TypedPerl.PerlType

type VarSet = (S.Set PerlTypeVars, S.Set RecsVar)

data PerlCType = PerlForall VarSet PerlType
type Context = [(PerlVars, PerlCType)]

type TypeError = String

data TypeContext = TypeContext {
  names :: TypeNames
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

asCTypeSchema :: Context -> PerlType -> PerlCType
asCTypeSchema ctx ty = PerlForall vs ty
  where
    vs = freeVar ty `minusVarSet` freeVarInContext ctx

freeVar :: PerlType -> VarSet
freeVar ty = foldType mapper ty
  where
    mapper = monoidMapper {
      var = var'
      , intRecNamed = intRecNamed'
      , strRecNamed = strRecNamed'
      }
    var' v = (S.singleton v, S.empty)
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
      , strRecNamed = strRecNamed' rvNames
      , intRecNamed = intRecNamed' rvNames
      }
    var' vNames v
      | v `S.member` vs = var nopMapper (vNames M.! v)
      | otherwise = var nopMapper v
    strRecNamed' rvNames v mmap
      | v `S.member` rvs = strRecNamed nopMapper (rvNames M.! v) mmap
      | otherwise = strRecNamed nopMapper v mmap
    intRecNamed' rvNames v mmap
      | v `S.member` rvs = intRecNamed nopMapper (rvNames M.! v) mmap
      | otherwise = intRecNamed nopMapper v mmap
