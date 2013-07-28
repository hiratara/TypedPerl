{-# LANGUAGE FlexibleContexts #-}
module TypedPerl.Inferance.TypeContext (
  TypeContext (..)
  , TypeError
  , freshName
  , initialTypeContext
  ) where
import Control.Monad.State.Class
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

typeNames :: TypeNames
typeNames = map (('a' :) . show) [(1 :: Integer)..]

initialTypeContext :: TypeContext
initialTypeContext = TypeContext {names = typeNames, context = []}
