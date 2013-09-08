module TypedPerl.PerlAST (
  PerlASTMapper (..)
  , foldAST
  , nopMapper
  ) where
import TypedPerl.Types
import qualified Data.Map as M
import Prelude hiding (seq);

data PerlASTMapper x y a b c = PerlASTMapper {
  ast :: y -> a -> x
  , info :: String -> Int -> Int -> y
  , declare :: PerlVars -> x -> a
  , int :: Integer -> a
  , str :: String -> a
  , var :: PerlVars -> a
  , implicitItem :: x -> Int -> a
  , op :: PerlBinOp -> x -> x -> a
  , obj :: b -> String -> a
  , objMapItem :: String -> x -> b -> b
  , objMapNil :: b
  , objItem :: x -> String -> a
  , objMeth :: x -> String -> c -> a
  , abstract :: x -> a
  , app :: x -> c -> a
  , appListCons :: x -> c -> c
  , appListNil :: c
  , seq :: x -> x -> a
  , package :: String -> x -> a
  }

nopMapper :: PerlASTMapper PerlAST SourceInfo PerlAST' (M.Map String PerlAST) [PerlAST]
nopMapper = PerlASTMapper {
  ast = PerlAST
  , info = SourceInfo
  , declare = PerlDeclare
  , int = PerlInt
  , str = PerlStr
  , var = PerlVar
  , implicitItem = PerlImplicitItem
  , op = PerlOp
  , obj = PerlObj
  , objMapItem = M.insert
  , objMapNil = M.empty
  , objItem = PerlObjItem
  , objMeth = PerlObjMeth
  , abstract = PerlAbstract
  , app = PerlApp
  , appListCons = (:)
  , appListNil = []
  , seq = PerlSeq
  , package = PerlPackage
  }

foldAST :: PerlASTMapper x y a b c -> PerlAST -> x
foldAST m (PerlAST anInfo ast') = ast m (foldInfo m anInfo) (foldAST' m ast')

foldInfo :: PerlASTMapper x y a b c -> SourceInfo -> y
foldInfo m (SourceInfo n l c) = info m n l c

foldAST' :: PerlASTMapper x y a b c -> PerlAST' -> a
foldAST' m (PerlDeclare v anAst) = declare m v (foldAST m anAst)
foldAST' m (PerlInt n) = int m n
foldAST' m (PerlStr s) = str m s
foldAST' m (PerlVar v) = var m v
foldAST' m (PerlImplicitItem anAst n) = implicitItem m (foldAST m anAst) n
foldAST' m (PerlOp o ast1 ast2) = op m o (foldAST m ast1) (foldAST m ast2)
foldAST' m (PerlObj ma s) = obj m (foldObjMap m ma) s
foldAST' m (PerlObjItem ast1 s) = objItem m (foldAST m ast1) s
foldAST' m (PerlObjMeth anAst s asts) = objMeth m (foldAST m anAst) s
                                               (foldAppList m asts)
foldAST' m (PerlAbstract anAst) = abstract m (foldAST m anAst)
foldAST' m (PerlApp anAst asts) = app m (foldAST m anAst) (foldAppList m asts)
foldAST' m (PerlSeq ast1 ast2) = seq m (foldAST m ast1) (foldAST m ast2)
foldAST' m (PerlPackage name anAst) = package m name (foldAST m anAst)

foldObjMap :: PerlASTMapper x y a b c -> M.Map String PerlAST -> b
foldObjMap m ma = M.foldWithKey (\k -> objMapItem m k . foldAST m)
                                (objMapNil m) ma

foldAppList :: PerlASTMapper x y a b c -> [PerlAST] -> c
foldAppList m asts = foldr (appListCons m . foldAST m) (appListNil m) asts
