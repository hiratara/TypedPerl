module TypedPerl.PerlAST (
  PerlASTMapper (..)
  , foldAST
  , nopMapper
  ) where
import TypedPerl.Types
import qualified Data.Map as M
import Prelude hiding (seq);

data PerlASTMapper a b c = PerlASTMapper {
  declare :: SourceInfo -> PerlVars -> a -> a
  , int :: SourceInfo -> Integer -> a
  , str :: SourceInfo -> String -> a
  , var :: SourceInfo -> PerlVars -> a
  , implicitItem :: SourceInfo -> a -> Int -> a
  , op :: SourceInfo -> PerlBinOp -> a -> a -> a
  , obj :: SourceInfo -> b -> String -> a
  , objMapItem :: String -> a -> b -> b
  , objMapNil :: b
  , objItem :: SourceInfo -> a -> String -> a
  , objMeth :: SourceInfo -> a -> String -> c -> a
  , abstract :: SourceInfo -> a -> a
  , app :: SourceInfo -> a -> c -> a
  , appListCons :: a -> c -> c
  , appListNil :: c
  , seq :: SourceInfo -> a -> a -> a
  , package :: SourceInfo -> String -> a -> a
  }

nopMapper :: PerlASTMapper PerlAST (M.Map String PerlAST) [PerlAST]
nopMapper = PerlASTMapper {
  declare = wrap2 PerlDeclare
  , int = wrap1 PerlInt
  , str = wrap1 PerlStr
  , var = wrap1 PerlVar
  , implicitItem = wrap2 PerlImplicitItem
  , op = wrap3 PerlOp
  , obj = wrap2 PerlObj
  , objMapItem = M.insert
  , objMapNil = M.empty
  , objItem = wrap2 PerlObjItem
  , objMeth = wrap3 PerlObjMeth
  , abstract = wrap1 PerlAbstract
  , app = wrap2 PerlApp
  , appListCons = (:)
  , appListNil = []
  , seq = wrap2 PerlSeq
  , package = wrap2 PerlPackage
  }
  where
    wrap1 f i = (PerlAST i) . f
    wrap2 f i = ((PerlAST i) .) . f
    wrap3 f i = (((PerlAST i) .) .) . f

foldAST :: PerlASTMapper a b c -> PerlAST -> a
foldAST m (PerlAST i anAst') = foldAST' anAst'
  where
    foldAST' (PerlDeclare v anAst) = declare m i v (foldAST m anAst)
    foldAST' (PerlInt n) = int m i n
    foldAST' (PerlStr s) = str m i s
    foldAST' (PerlVar v) = var m i v
    foldAST' (PerlImplicitItem anAst n) = implicitItem m i (foldAST m anAst) n
    foldAST' (PerlOp o ast1 ast2) = op m i o (foldAST m ast1) (foldAST m ast2)
    foldAST' (PerlObj ma s) = obj m i (foldObjMap m ma) s
    foldAST' (PerlObjItem ast1 s) = objItem m i (foldAST m ast1) s
    foldAST' (PerlObjMeth anAst s asts) = objMeth m i (foldAST m anAst) s
                                                      (foldAppList m asts)
    foldAST' (PerlAbstract anAst) = abstract m i (foldAST m anAst)
    foldAST' (PerlApp anAst asts) = app m i (foldAST m anAst) (foldAppList m asts)
    foldAST' (PerlSeq ast1 ast2) = seq m i (foldAST m ast1) (foldAST m ast2)
    foldAST' (PerlPackage name anAst) = package m i name (foldAST m anAst)

foldObjMap :: PerlASTMapper a b c -> M.Map String PerlAST -> b
foldObjMap m ma = M.foldWithKey (\k -> objMapItem m k . foldAST m)
                                (objMapNil m) ma

foldAppList :: PerlASTMapper a b c -> [PerlAST] -> c
foldAppList m asts = foldr (appListCons m . foldAST m) (appListNil m) asts
