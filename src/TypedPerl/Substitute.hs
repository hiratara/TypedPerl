{-# LANGUAGE FlexibleInstances #-}
module TypedPerl.Substitute (
  Substitute, SubstituteItem(..)
  , Substitutable (..)
  , emptySubst, compSubst, addSubst
) where
import qualified Data.Map as M
import qualified Data.Set as S
import TypedPerl.Inferance.TypeContext
import TypedPerl.PerlType
import TypedPerl.Types
import TypedPerl.Utils
data SubstituteItem =
  SubstType PerlTypeVars PerlType
  | SubstArgs RecsVar (PerlRecs Int)
  | SubstRecs RecsVar (PerlRecs String)
  deriving Show
type Substitute = (M.Map PerlTypeVars PerlType
                  , M.Map RecsVar (PerlRecs Int)
                  , M.Map RecsVar (PerlRecs String))

class Substitutable r where
  subst :: Substitute -> r -> r
  subst (ss1, ss2, ss3) r =
    flip (M.foldWithKey ((subst1 .) . SubstType)) ss1 .
    flip (M.foldWithKey ((subst1 .) . SubstArgs)) ss2 .
    flip (M.foldWithKey ((subst1 .) . SubstRecs)) ss3 $ r
  subst1 :: SubstituteItem -> r -> r
  subst1 s r = subst (singleton s) r

instance Substitutable PerlType where
  subst1 s = foldType (substMapper s)

instance Substitutable (PerlRecs Int) where
  subst1 s = foldRecInt (substMapper s)

instance Substitutable (PerlRecs String) where
  subst1 s = foldRecStr (substMapper s)

instance Substitutable (SubstituteItem) where
  subst ss = substSubst'
    where
      substSubst' (SubstType v ty) = SubstType v (subst ss ty)
      substSubst' (SubstArgs v reco) = SubstArgs v (subst ss reco)
      substSubst' (SubstRecs v reco) = SubstRecs v (subst ss reco)

instance Substitutable PerlCType where
  subst (s1, s2, s3) (PerlForall vs@(tvs, rvs) ty) =
    PerlForall vs (subst s' ty)
    where s' = (remove s1 tvs
                , remove s2 rvs
                , remove s3 rvs
                )
          remove m s = M.filterWithKey (\k -> (const . not . S.member k) s) m

newtype WrappedFunctor f a = WrappedFunctor {unWrap :: f a}

instance (Functor f, Substitutable r) => Substitutable (WrappedFunctor f r)
  where
    subst ss rs = WrappedFunctor (fmap (subst ss) (unWrap rs))

instance Substitutable r => Substitutable (M.Map k r) where
  subst = substOnFunctor

instance Substitutable r => Substitutable [r] where
  subst = substOnFunctor

instance Substitutable r => Substitutable (a, r) where
  subst = substOnFunctor

instance (Substitutable r1, Substitutable r2, Substitutable r3) =>
         Substitutable (r1, r2, r3) where
  subst s (x, y, z) = (subst s x, subst s y, subst s z)

substOnFunctor :: (Functor f, Substitutable r) => Substitute -> f r -> f r
substOnFunctor ss = unWrap . subst ss . WrappedFunctor

substMapper :: SubstituteItem ->
               PerlTypeMapper PerlType
                              (PerlRecs Int)    (M.Map Int PerlType)
                              (PerlRecs String) (M.Map String PerlType)
substMapper subs = nopMapper {
  var = substVar subs
  , fix = substFix subs
  , intRecNamed = substRecInt subs
  , strRecNamed = substRecStr subs
  }

substVar :: SubstituteItem -> PerlTypeVars -> PerlType
substVar (SubstType v' ty') v | v' == v = ty'
substVar _ v = var nopMapper v

substFix :: SubstituteItem -> PerlTypeVars -> PerlType -> PerlType
substFix (SubstType v' _) v _
  | v' == v = error ("[BUG]variable named " ++ show v
                     ++ "is duplicated.")
substFix _ v ty = fix nopMapper v ty

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

emptySubst :: Substitute
emptySubst = (M.empty, M.empty, M.empty)

infixr 6 `compSubst`
compSubst :: Substitute -> Substitute -> Substitute
compSubst s@(s1, s2, s3) (s1', s2', s3') = (s1 `M.union` subst s s1'
                                         , s2 `M.union` subst s s2'
                                         , s3 `M.union` subst s s3')

singleton :: SubstituteItem -> Substitute
singleton (SubstType v ty) = (M.singleton v ty, M.empty, M.empty)
singleton (SubstArgs v reco) = (M.empty, M.singleton v reco, M.empty)
singleton (SubstRecs v reco) = (M.empty, M.empty, M.singleton v reco)

infixr 6 `addSubst`
addSubst :: SubstituteItem -> Substitute -> Substitute
addSubst s1 ss = singleton s1 `compSubst` (subst1 s1 ss)
