module Ore.Utils (
    sameKeys
  , deleteKeys
  ) where
import qualified Data.Map as M
import Data.List

sameKeys :: Eq k => M.Map k v -> M.Map k v' -> [k]
sameKeys m1 m2 = intersect (M.keys m1) (M.keys m2)

deleteKeys :: Ord k => [k] -> M.Map k v -> M.Map k v
deleteKeys ks m = foldr M.delete m ks
