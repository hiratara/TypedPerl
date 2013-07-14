module Ore.Substitute (
  Substitute, SubstituteItem(..)
  , substType, substArgs
) where
import qualified Data.Map as M
import Ore.PerlType
import Ore.Types
import Ore.Utils
data SubstituteItem =
  SubstType PerlTypeVars PerlType
  | SubstArgs String PerlArgs
type Substitute = [SubstituteItem]

substType :: Substitute -> PerlType -> PerlType
substType ss ty = foldl (flip substType') ty ss

substType' :: SubstituteItem -> PerlType -> PerlType
substType' (SubstArgs x args) ty = mapType TypeVar substOne ty
  where
    substOne an@(ArgNamed x' _) = if x == x' then args else an
substType' (SubstType v' ty') ty = mapType substOne id ty
  where
    substOne v = if v == v' then ty' else TypeVar v

substArgs :: Substitute -> PerlArgs -> PerlArgs
substArgs ss ty = foldl (flip substArgs') ty ss

substArgs' :: SubstituteItem -> PerlArgs -> PerlArgs
substArgs' (SubstArgs x args) ty = mapArgs TypeVar substOne ty
  where
    substOne args'@(ArgNamed x' m)
      | x == x'   = argMerge args m
      | otherwise = args'
substArgs' (SubstType v' ty') ty = mapArgs substOne id ty
  where
    substOne v = if v == v' then ty' else TypeVar v

substTypeMap :: Substitute -> M.Map k PerlType -> M.Map k PerlType
substTypeMap ss = M.map (substType ss)

argMerge :: PerlArgs -> M.Map Int PerlType -> PerlArgs
argMerge (ArgEmpty m) m' = ArgEmpty (unsafeUnion m m')
argMerge (ArgNamed x m) m' = ArgNamed x (unsafeUnion m m')

unsafeUnion :: Ord k => M.Map k v -> M.Map k v -> M.Map k v
unsafeUnion m m' =
  if sameKeys m m' == []
     then M.union m m'
     else error "[BUG]2 other maps found"
