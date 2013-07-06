module Ore.Types (
  PerlTypeVars (..),
  PerlTypeBuiltins (..),
  PerlType (..),
  PerlVars (..),
  PerlAST (..),
  showPerlVars, showPerlAST,
  showPerlTypeVars, showPerlType
  ) where

data PerlTypeVars =
  TypeUnknown
  | TypeNamed String
  deriving (Show, Eq)

data PerlTypeBuiltins =
  TypeStr
  | TypeInt
  deriving (Show, Eq)

data PerlType =
  TypeVar PerlTypeVars
  | TypeBuiltin PerlTypeBuiltins
  | TypeArrow PerlType PerlType
  deriving (Show, Eq)

data PerlVars =
  VarSubImplicit
  | VarNamed String
  deriving (Show, Eq)

data PerlAST =
  PerlDeclare PerlVars PerlType PerlAST
  | PerlInt Integer
  | PerlStr String
  | PerlVar PerlVars
  | PerlOp String PerlAST PerlAST
  | PerlAbstract PerlAST
  | PerlApp PerlAST PerlAST
  | PerlSeq PerlAST PerlAST
  deriving Show

showPerlTypeVars :: PerlTypeVars -> String
showPerlTypeVars TypeUnknown = "?"
showPerlTypeVars (TypeNamed x) = x

showPerlTypeBuiltins :: PerlTypeBuiltins -> String
showPerlTypeBuiltins TypeInt = "Int"
showPerlTypeBuiltins TypeStr = "Str"

showPerlType :: PerlType -> String
showPerlType (TypeVar tyv) = showPerlTypeVars tyv
showPerlType (TypeBuiltin ty) = showPerlTypeBuiltins ty
showPerlType (TypeArrow ty1 ty2) = '(' : showPerlType ty1 ++ ") -> ("
                                   ++ showPerlType ty2 ++ ")"

showPerlVars :: PerlVars -> String
showPerlVars VarSubImplicit = "$_[0]"
showPerlVars (VarNamed x) = '$' : x

showPerlAST :: PerlAST -> String
showPerlAST (PerlInt n) = show n
showPerlAST (PerlStr x) = show x -- This is, though, Haskell's literal
showPerlAST (PerlVar t) = showPerlVars t
showPerlAST (PerlDeclare v _ t) = "my " ++ (showPerlVars v) ++
                                   " = (" ++ showPerlAST t ++ ")"
showPerlAST (PerlOp op t1 t2) = "(" ++ showPerlAST t1 ++ " "
                                ++ op ++ " " ++
                                showPerlAST t2 ++ ")"
showPerlAST (PerlAbstract t) = "sub {" ++ " " ++ showPerlAST t ++ " }"
showPerlAST (PerlApp t1 t2) = "(" ++ showPerlAST t1 ++
                              ")->(" ++ showPerlAST t2 ++ ")"
showPerlAST (PerlSeq t1 t2) = showPerlAST t1 ++ "; " ++ showPerlAST t2
