module Ore.Types (
  PerlType (..),
  PerlVars (..),
  PerlAST (..),
  showPerlVars, showPerlAST
  ) where

data PerlType =
  TypeUnknown
  | TypeInt
  | TypeArrow PerlType PerlType
  deriving (Show, Eq)

data PerlVars =
  VarSubImplicit
  deriving Show

data PerlAST =
  PerlInt Integer
  | PerlVar PerlVars PerlType
  | PerlOp String PerlAST PerlAST
  | PerlAbstract PerlAST
  | PerlApp PerlAST PerlAST
  deriving Show

showPerlVars :: PerlVars -> String
showPerlVars VarSubImplicit = "$_[0]"

showPerlAST :: PerlAST -> String
showPerlAST (PerlInt n) = show n
showPerlAST (PerlVar t _) = showPerlVars t
showPerlAST (PerlOp op t1 t2) = "(" ++ showPerlAST t1 ++ " "
                                ++ op ++ " " ++
                                showPerlAST t2 ++ ")"
showPerlAST (PerlAbstract t) = "sub {" ++ " " ++ showPerlAST t ++ " }"
showPerlAST (PerlApp t1 t2) = "(" ++ showPerlAST t1 ++
                              ")->(" ++ showPerlAST t2 ++ ")"
