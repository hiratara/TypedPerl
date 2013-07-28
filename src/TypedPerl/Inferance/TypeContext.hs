{-# LANGUAGE FlexibleContexts #-}
module TypedPerl.Inferance.TypeContext (
  TypeContext (..)
  , TypeError
  , freshType, freshRec
  , withContext
  , initialTypeContext
  ) where
import Control.Monad
import Control.Monad.State.Class
import qualified Data.Map as M
import TypedPerl.Types

type Context = [(PerlVars, PerlType)]

type TypeError = String

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
  curKeys <- liftM (map fst) (gets context)
  x <- modify (\tc -> tc {context = (f . context) tc}) >> mx
  resultCtx <- gets context
  modify (\tc -> tc {context = dropByTailKeys curKeys resultCtx})
  return x

dropByTailKeys :: Eq k => [k] -> [(k, v)] -> [(k, v)]
dropByTailKeys ks assoc = reverse $ dropByTailKeys' (reverse ks)
                                                    (reverse assoc)
  where
    dropByTailKeys' [] _ = []
    dropByTailKeys' (k':ks') ((k, v):as)
      | k' == k = (k, v):dropByTailKeys' ks' as
    dropByTailKeys' _ _ = error "[BUG]Don't touch existed Contexts"
