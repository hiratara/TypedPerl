module TypedPerl.PerlRecs (
  unionRec
  ) where
import qualified Data.Map as M
import TypedPerl.Types

unionRec :: Ord k => M.Map k PerlType -> PerlRecs k -> PerlRecs k
unionRec m r = r {recMap = (M.union (recMap r) m)}
