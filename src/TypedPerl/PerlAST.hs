module TypedPerl.PerlAST (
  PerlASTMapper (..)
  , foldAST
  , nopMapper
  ) where
import TypedPerl.Types
import qualified Data.Map as M
import Prelude hiding (seq);

data PerlASTMapper a b c = PerlASTMapper {
  subDeclare :: PerlVars -> a -> a
  , declare :: PerlVars -> a -> a
  , int :: Integer -> a
  , str :: String -> a
  , var :: PerlVars -> a
  , implicitItem :: Int -> a
  , op :: PerlBinOp -> a -> a -> a
  , obj :: b -> String -> a
  , objMapItem :: String -> a -> b -> b
  , objMapNil :: b
  , objItem :: a -> String -> a
  , abstract :: a -> a
  , app :: a -> c -> a
  , appListCons :: a -> c -> c
  , appListNil :: c
  , seq :: a -> a -> a
  }

nopMapper :: PerlASTMapper PerlAST (M.Map String PerlAST) [PerlAST]
nopMapper = PerlASTMapper {
  subDeclare = PerlSubDeclare
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
  , abstract = PerlAbstract
  , app = PerlApp
  , appListCons = (:)
  , appListNil = []
  , seq = PerlSeq
  }

foldAST :: PerlASTMapper a b c -> PerlAST -> a
foldAST m (PerlSubDeclare v ast) = subDeclare m v (foldAST m ast)
foldAST m (PerlDeclare v ast) = declare m v (foldAST m ast)
foldAST m (PerlInt n) = int m n
foldAST m (PerlStr s) = str m s
foldAST m (PerlVar v) = var m v
foldAST m (PerlImplicitItem n) = implicitItem m n
foldAST m (PerlOp o ast1 ast2) = op m o (foldAST m ast1) (foldAST m ast2)
foldAST m (PerlObj ma s) = obj m (foldObjMap m ma) s
foldAST m (PerlObjItem ast1 s) = objItem m (foldAST m ast1) s
foldAST m (PerlAbstract ast) = abstract m (foldAST m ast)
foldAST m (PerlApp ast asts) = app m (foldAST m ast) (foldAppList m asts)
foldAST m (PerlSeq ast1 ast2) = seq m (foldAST m ast1) (foldAST m ast2)

foldObjMap :: PerlASTMapper a b c -> M.Map String PerlAST -> b
foldObjMap m ma = M.foldWithKey (\k -> objMapItem m k . foldAST m)
                                (objMapNil m) ma

foldAppList :: PerlASTMapper a b c -> [PerlAST] -> c
foldAppList m asts = foldr (appListCons m . foldAST m) (appListNil m) asts
