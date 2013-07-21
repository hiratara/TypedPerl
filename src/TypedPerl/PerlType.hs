module TypedPerl.PerlType (
    varsMapType, varsMapRecs, varsFoldMapType, varsFoldMapRecs
  , mapType, mapRecs
  ) where
import qualified Data.Map as M
import Data.Monoid
import Data.Typeable
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

mapType :: (Typeable k) =>
           (PerlTypeVars -> PerlType) -> (PerlRecs k -> PerlRecs k) ->
           PerlType -> PerlType
mapType f _ (TypeVar v) = f v
mapType _ _ ty@(TypeUnknown) = ty
mapType _ _ ty@(TypeBuiltin _) = ty
mapType f g (TypeArg args) = TypeArg (mapRecs f g args)
mapType f g (TypeObj args) = TypeObj (mapRecs f g args)
mapType f g (TypeArrow t1 t2) = TypeArrow (mapType' t1) (mapType' t2)
  where mapType' = mapType f g

mapRecs :: (Typeable k, Typeable k') =>
           (PerlTypeVars -> PerlType) -> (PerlRecs k -> PerlRecs k) ->
           PerlRecs k' -> PerlRecs k'
mapRecs f g (RecNamed x m) = g' (RecNamed x (mapRecsMap f g m))
  where g' y = maybe y id $ cast y >>= cast . g
mapRecs f g (RecEmpty m) = RecEmpty (mapRecsMap f g m)

mapRecsMap :: (Typeable k, Typeable k') =>
              (PerlTypeVars -> PerlType) -> (PerlRecs k -> PerlRecs k) ->
              M.Map k' PerlType -> M.Map k' PerlType
mapRecsMap f g = M.map (mapType f g)
