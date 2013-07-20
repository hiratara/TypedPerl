module TypedPerl.Substitute (
  Substitute, SubstituteItem(..)
  , substType, substRecs
  , substRecsStr
) where
import qualified Data.Map as M
import TypedPerl.PerlType
import TypedPerl.Types
import TypedPerl.Utils
data SubstituteItem =
  SubstType PerlTypeVars PerlType
  | SubstArgs RecsVar (PerlRecs Int)
  | SubstRecs RecsVar (PerlRecs String)
type Substitute = [SubstituteItem]

substType :: Substitute -> PerlType -> PerlType
substType ss ty = foldl (flip substType') ty ss

substType' :: SubstituteItem -> PerlType -> PerlType
substType' (SubstArgs x args) ty = mapType TypeVar substOne id ty
  where
    substOne an@(RecNamed x' _) = if x == x' then args else an
-- Copied from SubstArgs
substType' (SubstRecs x args) ty = mapType TypeVar id substOne ty
  where
    substOne an@(RecNamed x' _) = if x == x' then args else an
substType' (SubstType v' ty') ty = mapType substOne id id ty
  where
    substOne v = if v == v' then ty' else TypeVar v

substRecs :: Substitute -> PerlRecs Int -> PerlRecs Int
substRecs ss ty = foldl (flip substRecs') ty ss

substRecs' :: SubstituteItem -> PerlRecs Int -> PerlRecs Int
substRecs' (SubstArgs x args) ty = mapRecs TypeVar substOne id ty
  where
    substOne args'@(RecNamed x' m)
      | x == x'   = argMerge args m
      | otherwise = args'
-- Copied from substArgs
substRecs' (SubstRecs x args) ty = mapRecs TypeVar id substOne ty
  where
    substOne args'@(RecNamed x' m)
      | x == x'   = argMerge args m
      | otherwise = args'
substRecs' (SubstType v' ty') ty = mapRecs substOne id id ty
  where
    substOne v = if v == v' then ty' else TypeVar v

-- Copied from substRecs
substRecsStr :: Substitute -> PerlRecs String -> PerlRecs String
substRecsStr ss ty = foldl (flip substRecsStr') ty ss

-- Copied from substRecs'
substRecsStr' :: SubstituteItem -> PerlRecs String -> PerlRecs String
substRecsStr' (SubstArgs x args) ty = mapRecsStr TypeVar substOne id ty
  where
    substOne args'@(RecNamed x' m)
      | x == x'   = argMerge args m
      | otherwise = args'
substRecsStr' (SubstType v' ty') ty = mapRecsStr substOne id id ty
  where
    substOne v = if v == v' then ty' else TypeVar v

argMerge :: Ord k => PerlRecs k -> M.Map k PerlType -> PerlRecs k
argMerge (RecEmpty m) m' = RecEmpty (unsafeUnion m m')
argMerge (RecNamed x m) m' = RecNamed x (unsafeUnion m m')

unsafeUnion :: Ord k => M.Map k v -> M.Map k v -> M.Map k v
unsafeUnion m m' =
  if sameKeys m m' == []
     then M.union m m'
     else error "[BUG]2 other maps found"