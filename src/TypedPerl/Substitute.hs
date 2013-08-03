{-# LANGUAGE FlexibleInstances #-}
module TypedPerl.Substitute (
  Substitute, SubstituteItem(..)
  , Substable (..)
  , compSubst
) where
import qualified Data.Map as M
import TypedPerl.Inferance.TypeContext
import TypedPerl.PerlType
import TypedPerl.Types
import TypedPerl.Utils
data SubstituteItem =
  SubstType PerlTypeVars PerlType
  | SubstArgs RecsVar (PerlRecs Int)
  | SubstRecs RecsVar (PerlRecs String)
type Substitute = [SubstituteItem]

class Substable r where
  subst :: Substitute -> r -> r
  subst ss r = foldl (flip subst1) r ss
  subst1 :: SubstituteItem -> r -> r
  subst1 s r = subst [s] r

instance Substable PerlType where
  subst1 s = foldType (substMapper s)

instance Substable (PerlRecs Int) where
  subst1 s = foldRecInt (substMapper s)

instance Substable (PerlRecs String) where
  subst1 s = foldRecStr (substMapper s)

instance Substable (SubstituteItem) where
  subst ss = substSubst'
    where
      substSubst' (SubstType v ty) = SubstType v (subst ss ty)
      substSubst' (SubstArgs v reco) = SubstArgs v (subst ss reco)
      substSubst' (SubstRecs v reco) = SubstRecs v (subst ss reco)

instance Substable PerlCType where
  subst s (PerlForall vs ty) = PerlForall vs (subst s ty)

newtype WrappedFunctor f a = WrappedFunctor {unWrap :: f a}

instance (Functor f, Substable r) => Substable (WrappedFunctor f r) where
  subst ss rs = WrappedFunctor (fmap (subst ss) (unWrap rs))

instance Substable r => Substable (M.Map k r) where
  subst = substOnFunctor

instance Substable r => Substable [r] where
  subst = substOnFunctor

instance Substable r => Substable (a, r) where
  subst = substOnFunctor

substOnFunctor :: (Functor f, Substable r) => Substitute -> f r -> f r
substOnFunctor ss = unWrap . subst ss . WrappedFunctor

substMapper :: SubstituteItem ->
               PerlTypeMapper PerlType
                              (PerlRecs Int)    (M.Map Int PerlType)
                              (PerlRecs String) (M.Map String PerlType)
substMapper subs = nopMapper {
  var = substVar subs
  , intRecNamed = substRecInt subs
  , strRecNamed = substRecStr subs
  }

substVar :: SubstituteItem -> PerlTypeVars -> PerlType
substVar (SubstType v' ty') v | v' == v = ty'
substVar _ v = var nopMapper v

substRecInt :: SubstituteItem -> RecsVar -> M.Map Int PerlType
               -> PerlRecs Int
substRecInt (SubstArgs v reco) v' m | v == v' = argMerge reco m
substRecInt _ v m = intRecNamed nopMapper v m

substRecStr :: SubstituteItem -> RecsVar -> M.Map String PerlType
               -> PerlRecs String
substRecStr (SubstRecs v reco) v' m | v == v' = argMerge reco m
substRecStr _ v m = strRecNamed nopMapper v m

argMerge :: Ord k => PerlRecs k -> M.Map k PerlType -> PerlRecs k
argMerge (RecEmpty m) m' = RecEmpty (unsafeUnion m m')
argMerge (RecNamed x m) m' = RecNamed x (unsafeUnion m m')

unsafeUnion :: Ord k => M.Map k v -> M.Map k v -> M.Map k v
unsafeUnion m m' =
  if sameKeys m m' == []
     then M.union m m'
     else error "[BUG]2 other maps found"

compSubst :: Substitute -> Substitute -> Substitute
compSubst s2 s1 = s2 ++ (subst s2 s1)
