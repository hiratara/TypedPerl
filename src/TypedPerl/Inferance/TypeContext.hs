{-# LANGUAGE FlexibleContexts #-}
module TypedPerl.Inferance.TypeContext (
  TypeContext (..)
  , PerlCType (..), asCType, asCTypeSchema, extractCType
  , TypeError
  , freshType, freshRec
  , withContext
  , initialTypeContext
  ) where
import Control.Monad
import Control.Monad.State.Class
import qualified Data.Map as M
import Data.Monoid
import TypedPerl.Types
import TypedPerl.PerlType
import TypedPerl.Substitute

type VarSet = ([PerlTypeVars], [RecsVar])

data PerlCType = PerlForall VarSet PerlType
type Context = [(PerlVars, PerlCType)]

type TypeError = String

data TypeContext = TypeContext {
  names :: TypeNames
  , context :: Context
  }
type TypeNames = [String]

asCType :: PerlType -> PerlCType
asCType = PerlForall ([], [])

freshName :: MonadState TypeContext m => m String
freshName = do
  name <- gets (head . names)
  modify (\s -> s {names = (tail . names) s})
  return name

freshType :: MonadState TypeContext m => m PerlType
freshType = liftM (TypeVar . TypeNamed) freshName

freshRec :: MonadState TypeContext m => m (PerlRecs k)
freshRec = liftM (flip RecNamed M.empty) freshName

typeNames :: TypeNames
typeNames = map (('a' :) . show) [(1 :: Integer)..]

initialTypeContext :: TypeContext
initialTypeContext = TypeContext {names = typeNames, context = []}

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
    var' v = (v:[], [])
    intRecNamed' rv vss = ([], rv:[]) <> vss
    strRecNamed' rv vss = ([], rv:[]) <> vss

freeVarInCType :: PerlCType -> VarSet
freeVarInCType (PerlForall vs ty) = freeVar ty `minusVarSet` vs

freeVarInContext :: Context -> VarSet -- TODO: Simplify
freeVarInContext ctx = foldr (\x y -> y `mappend` (freeVarInCType . snd $ x)) mempty ctx

minusVarSet :: VarSet -> VarSet -> VarSet
minusVarSet (rvs1, vs1) (rvs2, vs2) = (minus rvs1 rvs2, minus vs1 vs2)
  where
    minus xs ys = filter (not . (`elem` ys)) xs

extractCType :: MonadState TypeContext m => PerlCType -> m PerlType
extractCType (PerlForall (vs, rvs) ty) = do
  vNames <- foldr (\v mm -> do m <- mm
                               n <- freshName
                               return (M.insert v (TypeNamed n) m))
                  (return M.empty) vs
  rvNames <- foldr (\v mm -> do m <- mm
                                v' <- freshName
                                return (M.insert v v' m))
                  (return M.empty) rvs
  return (foldType (mapper vNames rvNames) ty)
  where
    mapper vNames rvNames = nopMapper {
      var = var' vNames
      , strRecNamed = strRecNamed' rvNames
      , intRecNamed = intRecNamed' rvNames
      }
    var' vNames v
      | v `elem` vs = var nopMapper (vNames M.! v)
      | otherwise = var nopMapper v
    strRecNamed' rvNames v mmap
      | v `elem` rvs = strRecNamed nopMapper (rvNames M.! v) mmap
      | otherwise = strRecNamed nopMapper v mmap
    intRecNamed' rvNames v mmap
      | v `elem` rvs = intRecNamed nopMapper (rvNames M.! v) mmap
      | otherwise = intRecNamed nopMapper v mmap

instance Substable PerlCType where
  subst s (PerlForall vs ty) = PerlForall vs (subst s ty)
