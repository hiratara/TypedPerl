module TypedPerl.PerlType (
    varsMapType, varsMapRecs, varsFoldMapType, varsFoldMapRecs
  , mapType, mapRecs
  , mapRecsStr
  ) where
import qualified Data.Map as M
import Data.Monoid
import TypedPerl.Types

varsMapType :: (PerlTypeVars -> a) -> (PerlRecs Int -> a) ->
               (PerlRecs String -> a) -> PerlType -> [a]
varsMapType f _ _ (TypeVar v) = f v:[]
varsMapType _ _ _ (TypeUnknown) = []
varsMapType _ _ _ (TypeBuiltin _) = []
varsMapType f g h (TypeArg args) = varsMapRecs f g h args
varsMapType f g h (TypeObj obj) = varsMapRecsStr f g h obj
varsMapType f g h (TypeArrow t1 t2) = mapType' t1 ++ mapType' t2
  where mapType' = varsMapType f g h

varsMapRecs :: (PerlTypeVars -> a) -> (PerlRecs Int -> a) ->
               (PerlRecs String -> a) -> PerlRecs Int -> [a]
varsMapRecs f g h v@(RecNamed _ m) = g v : varsMapRecsMap f g h m
varsMapRecs f g h (RecEmpty m) = varsMapRecsMap f g h m

varsMapRecsMap :: (PerlTypeVars -> a) -> (PerlRecs Int -> a) ->
                  (PerlRecs String -> a) -> M.Map Int PerlType -> [a]
varsMapRecsMap f g h m = concat . map (varsMapType f g h) $ M.elems m

-- Copied from varsMapRecs
varsMapRecsStr :: (PerlTypeVars -> a) -> (PerlRecs Int -> a) ->
               (PerlRecs String -> a) -> PerlRecs String -> [a]
varsMapRecsStr f g h v@(RecNamed _ m) = h v : varsMapRecsMapStr f g h m
varsMapRecsStr f g h (RecEmpty m) = varsMapRecsMapStr f g h m

-- Copied from varsMapRecsMap
varsMapRecsMapStr :: (PerlTypeVars -> a) -> (PerlRecs Int -> a) ->
                  (PerlRecs String -> a) -> M.Map String PerlType -> [a]
varsMapRecsMapStr f g h m = concat . map (varsMapType f g h) $ M.elems m

varsFoldMapType :: Monoid a =>
               (PerlTypeVars -> a) -> (PerlRecs Int -> a) ->
               (PerlRecs String -> a) -> PerlType -> a
varsFoldMapType f g h = foldr mappend mempty . varsMapType f g h

varsFoldMapRecs :: Monoid a =>
                   (PerlTypeVars -> a) -> (PerlRecs Int -> a) ->
                   (PerlRecs String -> a) -> PerlRecs Int -> a
varsFoldMapRecs f g h = foldr mappend mempty . varsMapRecs f g h

mapType :: (PerlTypeVars -> PerlType) -> (PerlRecs Int -> PerlRecs Int) ->
           (PerlRecs String -> PerlRecs String) -> PerlType -> PerlType
mapType f _ _ (TypeVar v) = f v
mapType _ _ _ ty@(TypeUnknown) = ty
mapType _ _ _ ty@(TypeBuiltin _) = ty
mapType f g h (TypeArg args) = TypeArg (mapRecs f g h args)
mapType f g h (TypeObj args) = TypeObj (mapRecsStr f g h args)
mapType f g h (TypeArrow t1 t2) = TypeArrow (mapType' t1) (mapType' t2)
  where mapType' = mapType f g h

mapRecs :: (PerlTypeVars -> PerlType) -> (PerlRecs Int -> PerlRecs Int) ->
           (PerlRecs String -> PerlRecs String) -> PerlRecs Int -> PerlRecs Int
mapRecs f g h (RecNamed x m) = g (RecNamed x (mapRecsMap f g h m))
mapRecs f g h (RecEmpty m) = RecEmpty (mapRecsMap f g h m)

mapRecsMap :: (PerlTypeVars -> PerlType) -> (PerlRecs Int -> PerlRecs Int) ->
              (PerlRecs String -> PerlRecs String) ->
              M.Map Int PerlType -> M.Map Int PerlType
mapRecsMap f g h = M.map (mapType f g h)

-- Copied from mapRecs
mapRecsStr :: (PerlTypeVars -> PerlType) -> (PerlRecs Int -> PerlRecs Int) ->
           (PerlRecs String -> PerlRecs String) -> PerlRecs String -> PerlRecs String
mapRecsStr f g h (RecNamed x m) = h (RecNamed x (mapRecsMapStr f g h m))
mapRecsStr f g h (RecEmpty m) = RecEmpty (mapRecsMapStr f g h m)

-- Copied from mapRecsMap
mapRecsMapStr :: (PerlTypeVars -> PerlType) -> (PerlRecs Int -> PerlRecs Int) ->
              (PerlRecs String -> PerlRecs String) ->
              M.Map String PerlType -> M.Map String PerlType
mapRecsMapStr f g h = M.map (mapType f g h)
