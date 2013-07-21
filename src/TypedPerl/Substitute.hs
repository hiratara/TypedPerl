module TypedPerl.Substitute (
  Substitute, SubstituteItem(..)
  , substType, substRecs
) where
import qualified Data.Map as M
import Data.Typeable
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
substType' (SubstArgs x args) ty = substTypeByRecs x args ty
substType' (SubstRecs x args) ty = substTypeByRecs x args ty
substType' (SubstType v' ty') ty = mapType substOne nop ty
  where
    substOne v = if v == v' then ty' else TypeVar v

substTypeByRecs :: (Typeable k, Ord k) =>
                   RecsVar -> PerlRecs k -> PerlType -> PerlType
substTypeByRecs x args ty = mapType TypeVar substOne ty
  where
    substOne an@(RecNamed x' _) = if x == x' then args else an

substRecs :: Typeable k => Substitute -> PerlRecs k -> PerlRecs k
substRecs ss ty = foldl (flip substRecs') ty ss

substRecs' :: Typeable k => SubstituteItem -> PerlRecs k -> PerlRecs k
substRecs' (SubstArgs x args) ty = substRecsByRecs x args ty
substRecs' (SubstRecs x args) ty = substRecsByRecs x args ty
substRecs' (SubstType v' ty') ty = mapRecs substOne nop ty
  where
    substOne v = if v == v' then ty' else TypeVar v

substRecsByRecs :: (Typeable k, Ord k, Typeable k') =>
                     RecsVar -> PerlRecs k -> PerlRecs k' -> PerlRecs k'
substRecsByRecs x args ty = mapRecs TypeVar substOne ty
  where
    substOne args'@(RecNamed x' m)
      | x == x'   = argMerge args m
      | otherwise = args'

argMerge :: Ord k => PerlRecs k -> M.Map k PerlType -> PerlRecs k
argMerge (RecEmpty m) m' = RecEmpty (unsafeUnion m m')
argMerge (RecNamed x m) m' = RecNamed x (unsafeUnion m m')

unsafeUnion :: Ord k => M.Map k v -> M.Map k v -> M.Map k v
unsafeUnion m m' =
  if sameKeys m m' == []
     then M.union m m'
     else error "[BUG]2 other maps found"

-- Need specify an instance of Typeable
nop :: PerlRecs () -> PerlRecs ()
nop = id
