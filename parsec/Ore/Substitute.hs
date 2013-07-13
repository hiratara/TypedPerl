module Ore.Substitute (
  Substitute, SubstituteItem(..)
  , substType, substArgs
) where
import qualified Data.Map as M
import Ore.Types
import Ore.Utils
data SubstituteItem =
  SubstType PerlTypeVars PerlType
  | SubstArgs String PerlArgs
type Substitute = [SubstituteItem]

substType :: Substitute -> PerlType -> PerlType
substType [] ty = ty
substType (s:ss) ty@(TypeVar tyV) = case s of
  SubstType tyV' ty' | tyV == tyV' -> substType ss ty'
  _                                -> substType ss ty
substType _ TypeUnknown = TypeUnknown
substType _ b@(TypeBuiltin _) = b
substType ss (TypeArg x) = TypeArg (substArgs ss x)
substType ss (TypeArrow ty1 ty2) =
  TypeArrow (substType ss ty1) (substType ss ty2)

substArgs :: Substitute -> PerlArgs -> PerlArgs
substArgs [] args = args
substArgs ss (ArgEmpty m) = ArgEmpty (substTypeMap ss m)
substArgs (s:ss) (ArgNamed x m) = case s of
  SubstArgs x' args | x == x' -> substArgs ss (argMerge args m)
  _                           -> substArgs ss
                                           (ArgNamed x (substTypeMap (s:ss) m))
-- substArgs ss (ArgNamed x m) = ArgNamed x (substTypeMap ss m)

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
