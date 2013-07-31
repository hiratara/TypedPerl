{-# LANGUAGE FlexibleContexts #-}
module TypedPerl.Inferance.TypeContext (
  TypeContext (..)
  , PerlCType (..), asCType, extractCType
  , TypeError
  , freshType, freshRec
  , withContext
  , initialTypeContext
  ) where
import Control.Monad
import Control.Monad.State.Class
import qualified Data.Map as M
import TypedPerl.Types
import TypedPerl.PerlType

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

extractCType :: MonadState TypeContext m => PerlCType -> m PerlType
extractCType (PerlForall (vs, rvs) ty) = foldType mapper ty
  where
    mapper = nopMapperM {
      var = var'
      , strRecNamed = strRecNamed'
      , intRecNamed = intRecNamed'
      }
    var' v
      | v `elem` vs = do
        name <- freshName
        var nopMapperM (TypeNamed name)
      | otherwise = var nopMapperM v
    strRecNamed' v mmap
      | v `elem` rvs = do
        v' <- freshName
        strRecNamed nopMapperM v' mmap
      | otherwise = strRecNamed nopMapperM v mmap
    intRecNamed' v mmap
      | v `elem` rvs = do
        v' <- freshName
        intRecNamed nopMapperM v' mmap
      | otherwise = intRecNamed nopMapperM v mmap
