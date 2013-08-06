module TypedPerl.PerlAST (
  PerlASTMapper (..)
  , foldAST
  , nopMapper
  ) where
import TypedPerl.Types
import qualified Data.Map as M
import Prelude hiding (seq);

data PerlASTMapper a b c = PerlASTMapper {
  declare :: PerlVars -> a -> a
  , int :: Integer -> a
  , str :: String -> a
  , var :: PerlVars -> a
  , implicitItem :: a -> Int -> a
  , op :: PerlBinOp -> a -> a -> a
  , obj :: b -> String -> a
  , objMapItem :: String -> a -> b -> b
  , objMapNil :: b
  , objItem :: a -> String -> a
  , objMeth :: a -> String -> c -> a
  , abstract :: a -> a
  , app :: a -> c -> a
  , appListCons :: a -> c -> c
  , appListNil :: c
  , seq :: a -> a -> a
  , package :: String -> a -> a
  }

nopMapper :: PerlASTMapper PerlAST (M.Map String PerlAST) [PerlAST]
nopMapper = PerlASTMapper {
  declare = PerlDeclare
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

foldAST :: PerlASTMapper a b c -> PerlAST -> a
foldAST m (PerlDeclare v ast) = declare m v (foldAST m ast)
foldAST m (PerlInt n) = int m n
foldAST m (PerlStr s) = str m s
foldAST m (PerlVar v) = var m v
foldAST m (PerlImplicitItem ast n) = implicitItem m (foldAST m ast) n
foldAST m (PerlOp o ast1 ast2) = op m o (foldAST m ast1) (foldAST m ast2)
foldAST m (PerlObj ma s) = obj m (foldObjMap m ma) s
foldAST m (PerlObjItem ast1 s) = objItem m (foldAST m ast1) s
foldAST m (PerlObjMeth ast s asts) = objMeth m (foldAST m ast) s
                                               (foldAppList m asts)
foldAST m (PerlAbstract ast) = abstract m (foldAST m ast)
foldAST m (PerlApp ast asts) = app m (foldAST m ast) (foldAppList m asts)
foldAST m (PerlSeq ast1 ast2) = seq m (foldAST m ast1) (foldAST m ast2)
foldAST m (PerlPackage name ast) = package m name (foldAST m ast)

foldObjMap :: PerlASTMapper a b c -> M.Map String PerlAST -> b
foldObjMap m ma = M.foldWithKey (\k -> objMapItem m k . foldAST m)
                                (objMapNil m) ma

foldAppList :: PerlASTMapper a b c -> [PerlAST] -> c
foldAppList m asts = foldr (appListCons m . foldAST m) (appListNil m) asts
