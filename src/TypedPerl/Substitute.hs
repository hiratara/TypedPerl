{-# LANGUAGE FlexibleInstances #-}
module TypedPerl.Substitute (
  Substitute, SubstituteItem(..), subst
) where
import qualified Data.Map as M
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
  subst1 (SubstType v ty) (TypeVar v') | v == v' = ty
  subst1 s (TypeArg record) = TypeArg (subst1 s record)
  subst1 s (TypeObj record) = TypeObj (subst1 s record)
  subst1 s (TypeArrow ty1 ty2) = TypeArrow (subst1 s ty1) (subst1 s ty2)
  subst1 _ ty = ty

instance Substable (PerlRecs Int) where
  subst1 s@(SubstArgs v r) (RecNamed v' m) | v == v' = argMerge r (subst1 s m)
  subst1 s (RecNamed v m) = RecNamed v (subst1 s m)
  subst1 s (RecEmpty m) = RecEmpty (subst1 s m)

instance Substable (PerlRecs String) where
  subst1 s@(SubstRecs v r) (RecNamed v' m) | v == v' = argMerge r (subst1 s m)
  subst1 s (RecNamed v m) = RecNamed v (subst1 s m)
  subst1 s (RecEmpty m) = RecEmpty (subst1 s m)

instance Substable r => Substable (M.Map k r) where
  subst ss rs = M.map (subst ss) rs

argMerge :: Ord k => PerlRecs k -> M.Map k PerlType -> PerlRecs k
argMerge (RecEmpty m) m' = RecEmpty (unsafeUnion m m')
argMerge (RecNamed x m) m' = RecNamed x (unsafeUnion m m')

unsafeUnion :: Ord k => M.Map k v -> M.Map k v -> M.Map k v
unsafeUnion m m' =
  if sameKeys m m' == []
     then M.union m m'
     else error "[BUG]2 other maps found"
