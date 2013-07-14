module Ore.PerlType (
  varsMapType, varsMapArgs, varsFoldMapType, varsFoldMapArgs
  ) where
import qualified Data.Map as M
import Data.Monoid
import Ore.Types

varsMapType :: (PerlTypeVars -> a) -> (PerlArgs -> a) -> PerlType -> [a]
varsMapType f _ (TypeVar v) = f v:[]
varsMapType _ _ (TypeUnknown) = []
varsMapType _ _ (TypeBuiltin _) = []
varsMapType f g (TypeArg args) = varsMapArgs f g args
varsMapType f g (TypeArrow t1 t2) = mapType' t1 ++ mapType' t2
  where mapType' = varsMapType f g

varsMapArgs :: (PerlTypeVars -> a) -> (PerlArgs -> a) -> PerlArgs -> [a]
varsMapArgs f g v@(ArgNamed _ m) = g v : varsMapArgsMap f g m
varsMapArgs f g (ArgEmpty m) = varsMapArgsMap f g m

varsMapArgsMap :: (PerlTypeVars -> a) -> (PerlArgs -> a) -> M.Map k PerlType -> [a]
varsMapArgsMap f g m = concat . map (varsMapType f g) $ M.elems m

varsFoldMapType :: Monoid a =>
               (PerlTypeVars -> a) -> (PerlArgs -> a) -> PerlType -> a
varsFoldMapType f g = foldr mappend mempty . varsMapType f g

varsFoldMapArgs :: Monoid a =>
               (PerlTypeVars -> a) -> (PerlArgs -> a) -> PerlArgs -> a
varsFoldMapArgs f g = foldr mappend mempty . varsMapArgs f g
