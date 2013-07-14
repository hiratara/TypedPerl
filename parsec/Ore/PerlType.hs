module Ore.PerlType (
  mapType, mapArgs, foldMapType, foldMapArgs
  ) where
import qualified Data.Map as M
import Data.Monoid
import Ore.Types

mapType :: (PerlTypeVars -> a) -> (PerlArgs -> a) -> PerlType -> [a]
mapType f _ (TypeVar v) = f v:[]
mapType _ _ (TypeUnknown) = []
mapType _ _ (TypeBuiltin _) = []
mapType f g (TypeArg args) = mapArgs f g args
mapType f g (TypeArrow t1 t2) = mapType' t1 ++ mapType' t2
  where mapType' = mapType f g

mapArgs :: (PerlTypeVars -> a) -> (PerlArgs -> a) -> PerlArgs -> [a]
mapArgs f g v@(ArgNamed _ m) = g v : mapArgsMap f g m
mapArgs f g (ArgEmpty m) = mapArgsMap f g m

foldMapType :: Monoid a =>
               (PerlTypeVars -> a) -> (PerlArgs -> a) -> PerlType -> a
foldMapType f g = foldr mappend mempty . mapType f g

foldMapArgs :: Monoid a =>
               (PerlTypeVars -> a) -> (PerlArgs -> a) -> PerlArgs -> a
foldMapArgs f g = foldr mappend mempty . mapArgs f g

mapArgsMap :: (PerlTypeVars -> a) -> (PerlArgs -> a) -> M.Map k PerlType -> [a]
mapArgsMap f g m = concat . map (mapType f g) $ M.elems m
