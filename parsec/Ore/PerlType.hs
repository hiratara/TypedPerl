module Ore.PerlType (
    varsMapType, varsMapRecs, varsFoldMapType, varsFoldMapRecs
  , mapType, mapRecs
  ) where
import qualified Data.Map as M
import Data.Monoid
import Ore.Types

varsMapType :: (PerlTypeVars -> a) -> (PerlRecs Int -> a) -> PerlType -> [a]
varsMapType f _ (TypeVar v) = f v:[]
varsMapType _ _ (TypeUnknown) = []
varsMapType _ _ (TypeBuiltin _) = []
varsMapType f g (TypeArg args) = varsMapRecs f g args
varsMapType f g (TypeArrow t1 t2) = mapType' t1 ++ mapType' t2
  where mapType' = varsMapType f g

varsMapRecs :: (PerlTypeVars -> a) -> (PerlRecs Int -> a) -> PerlRecs Int -> [a]
varsMapRecs f g v@(RecNamed _ m) = g v : varsMapRecsMap f g m
varsMapRecs f g (RecEmpty m) = varsMapRecsMap f g m

varsMapRecsMap :: (PerlTypeVars -> a) -> (PerlRecs Int -> a) -> M.Map Int PerlType -> [a]
varsMapRecsMap f g m = concat . map (varsMapType f g) $ M.elems m

varsFoldMapType :: Monoid a =>
               (PerlTypeVars -> a) -> (PerlRecs Int -> a) -> PerlType -> a
varsFoldMapType f g = foldr mappend mempty . varsMapType f g

varsFoldMapRecs :: Monoid a =>
                   (PerlTypeVars -> a) -> (PerlRecs Int -> a) -> PerlRecs Int -> a
varsFoldMapRecs f g = foldr mappend mempty . varsMapRecs f g

mapType :: (PerlTypeVars -> PerlType) -> (PerlRecs Int -> PerlRecs Int)
           -> PerlType -> PerlType
mapType f _ (TypeVar v) = f v
mapType _ _ ty@(TypeUnknown) = ty
mapType _ _ ty@(TypeBuiltin _) = ty
mapType f g (TypeArg args) = TypeArg (mapRecs f g args)
mapType f g (TypeArrow t1 t2) = TypeArrow (mapType' t1) (mapType' t2)
  where mapType' = mapType f g

mapRecs :: (PerlTypeVars -> PerlType) -> (PerlRecs Int -> PerlRecs Int)
           -> PerlRecs Int -> PerlRecs Int
mapRecs f g (RecNamed x m) = g (RecNamed x (mapRecsMap f g m))
mapRecs f g (RecEmpty m) = RecEmpty (mapRecsMap f g m)

mapRecsMap :: (PerlTypeVars -> PerlType) -> (PerlRecs Int -> PerlRecs Int)
              -> M.Map Int PerlType -> M.Map Int PerlType
mapRecsMap f g = M.map (mapType f g)
