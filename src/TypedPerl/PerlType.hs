{-# LANGUAGE RankNTypes #-}
module TypedPerl.PerlType (
    varsFoldMapType
    , PerlTypeMapper(..), foldType, foldRecInt, foldRecStr
    , nopMapper
  ) where
import qualified Data.Map as M
import Data.Monoid
import TypedPerl.Types

varsMapType :: (PerlTypeVars -> a) -> (forall k.PerlRecs k -> a) -> PerlType -> [a]
varsMapType f _ (TypeVar v) = f v:[]
varsMapType _ _ (TypeUnknown) = []
varsMapType _ _ (TypeBuiltin _) = []
varsMapType f g (TypeArg args) = varsMapRecs f g args
varsMapType f g (TypeObj ob) = varsMapRecs f g ob
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

data PerlTypeMapper a b1 b2 c1 c2 = PerlTypeMapper {
  var :: PerlTypeVars -> a
  , unknown :: a
  , builtin :: PerlTypeBuiltins -> a
  , arg :: b1 -> a
  , obj :: c1 -> a
  , arrow :: a -> a -> a
  , intRecEmpty :: b2 -> b1
  , intRecNamed :: RecsVar -> b2 -> b1
  , intMapItem :: Int -> a-> b2 -> b2
  , intMapNil :: b2
  , strRecEmpty :: c2 -> c1
  , strRecNamed :: RecsVar -> c2 -> c1
  , strMapItem :: String -> a -> c2 -> c2
  , strMapNil :: c2
  }

nopMapper :: PerlTypeMapper PerlType (PerlRecs Int)    (M.Map Int PerlType)
                                     (PerlRecs String) (M.Map String PerlType)
nopMapper = PerlTypeMapper {
  var = TypeVar
  , unknown = TypeUnknown
  , builtin = TypeBuiltin
  , arg = TypeArg
  , obj = TypeObj
  , arrow = TypeArrow
  , intRecEmpty = RecEmpty
  , intRecNamed = RecNamed
  , intMapItem = M.insert
  , intMapNil = M.empty
  , strRecEmpty = RecEmpty
  , strRecNamed = RecNamed
  , strMapItem = M.insert
  , strMapNil = M.empty
}

foldType :: PerlTypeMapper a b1 b2 c1 c2 -> PerlType -> a
foldType mapper (TypeVar v) = var mapper v
foldType mapper (TypeUnknown) = unknown mapper
foldType mapper (TypeBuiltin b) = builtin mapper b
foldType mapper (TypeArg a) = arg mapper (foldRecInt mapper a)
foldType mapper (TypeObj o) = obj mapper (foldRecStr mapper o)
foldType mapper (TypeArrow ty1 ty2) = arrow mapper (foldType mapper ty1)
                                                   (foldType mapper ty2)

foldRecInt :: PerlTypeMapper a b1 b2 c1 c2 -> PerlRecs Int -> b1
foldRecInt mapper (RecEmpty xs) = intRecEmpty mapper xs'
  where
    xs' = M.foldWithKey (\k -> intMapItem mapper k . foldType mapper)
                        (intMapNil mapper) xs
foldRecInt mapper (RecNamed x xs) = intRecNamed mapper x xs'
  where
    xs' = M.foldWithKey (\k -> intMapItem mapper k . foldType mapper)
                        (intMapNil mapper) xs

foldRecStr :: PerlTypeMapper a b1 b2 c1 c2 -> PerlRecs String -> c1
foldRecStr mapper (RecEmpty xs) = strRecEmpty mapper xs'
  where
    xs' = M.foldWithKey (\k -> strMapItem mapper k . foldType mapper)
                        (strMapNil mapper) xs
foldRecStr mapper (RecNamed x xs) = strRecNamed mapper x xs'
  where
    xs' = M.foldWithKey (\k -> strMapItem mapper k . foldType mapper)
                        (strMapNil mapper) xs
