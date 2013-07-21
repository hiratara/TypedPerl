{-# LANGUAGE RankNTypes #-}
module TypedPerl.PerlType (
    varsFoldMapType
  ) where
import qualified Data.Map as M
import Data.Monoid
import TypedPerl.Types

varsMapType :: (PerlTypeVars -> a) -> (forall k.PerlRecs k -> a) -> PerlType -> [a]
varsMapType f _ (TypeVar v) = f v:[]
varsMapType _ _ (TypeUnknown) = []
varsMapType _ _ (TypeBuiltin _) = []
varsMapType f g (TypeArg args) = varsMapRecs f g args
varsMapType f g (TypeObj obj) = varsMapRecs f g obj
varsMapType f g (TypeArrow t1 t2) = mapType' t1 ++ mapType' t2
  where mapType' = varsMapType f g

varsMapRecs :: (PerlTypeVars -> a) -> (forall k.PerlRecs k -> a) -> PerlRecs k' -> [a]
varsMapRecs f g v@(RecNamed _ m) = g v : varsMapRecsMap f g m

varsMapRecs f g (RecEmpty m) = varsMapRecsMap f g m

varsMapRecsMap :: (PerlTypeVars -> a) -> (forall k.PerlRecs k -> a) ->
                  M.Map k' PerlType -> [a]
varsMapRecsMap f g m = concat . map (varsMapType f g) $ M.elems m

varsFoldMapType :: Monoid a =>
               (PerlTypeVars -> a) -> (forall k.PerlRecs k -> a) -> PerlType -> a
varsFoldMapType f g = foldr mappend mempty . varsMapType f g
