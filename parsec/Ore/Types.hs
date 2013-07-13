module Ore.Types (
  PerlTypeVars (..),
  PerlArgs (..),
  PerlTypeBuiltins (..),
  PerlType (..),
  PerlVars (..),
  PerlBinOp (..),
  PerlAST (..),
  showPerlVars, showPerlAST,
  showPerlTypeVars, showPerlType
  ) where
import Data.Map (Map)

data PerlTypeVars =
  TypeNamed String
  deriving (Show, Eq)

data PerlArgs =
  ArgEmpty (Map Int PerlType)
  | ArgNamed String (Map Int PerlType)
  deriving (Show, Eq)

data PerlTypeBuiltins =
  TypeStr
  | TypeInt
  deriving (Show, Eq)

data PerlType =
  TypeVar PerlTypeVars
  | TypeUnknown
  | TypeBuiltin PerlTypeBuiltins
  | TypeArg PerlArgs
  | TypeArrow PerlType PerlType
  deriving (Show, Eq)

data PerlVars =
  VarSubImplicit
  | VarNamed String
  | VarSub String
  deriving (Show, Eq)

data PerlBinOp = PerlBinOp {
  symbol :: String
  , leftType :: PerlType
  , rightType :: PerlType
  , returnType :: PerlType
} deriving (Show, Eq)

data PerlAST =
  PerlSubDeclare PerlVars PerlAST
  | PerlDeclare PerlVars PerlAST
  | PerlInt Integer
  | PerlStr String
  | PerlVar PerlVars
  | PerlImplicitItem Int
  | PerlOp PerlBinOp PerlAST PerlAST
  | PerlAbstract PerlAST
  | PerlApp PerlAST [PerlAST]
  | PerlSeq PerlAST PerlAST
  deriving (Show, Eq)

showPerlTypeVars :: PerlTypeVars -> String
showPerlTypeVars (TypeNamed x) = x

showArgsMap :: Map Int PerlType -> String
showArgsMap = show

showPerlArgs :: PerlArgs -> String
showPerlArgs (ArgEmpty m) = showArgsMap m -- TODO
showPerlArgs (ArgNamed v m) = v ++ "⊕" ++ showArgsMap m

showPerlTypeBuiltins :: PerlTypeBuiltins -> String
showPerlTypeBuiltins TypeInt = "Int"
showPerlTypeBuiltins TypeStr = "Str"

showPerlType :: PerlType -> String
showPerlType (TypeVar tyv) = showPerlTypeVars tyv
showPerlType TypeUnknown = "?"
showPerlType (TypeBuiltin ty) = showPerlTypeBuiltins ty
showPerlType (TypeArrow ty1 ty2) = '(' : showPerlType ty1 ++ ") -> ("
                                   ++ showPerlType ty2 ++ ")"
showPerlType (TypeArg r) = showPerlArgs r

showPerlVars :: PerlVars -> String
showPerlVars VarSubImplicit = "@_"
showPerlVars (VarNamed x) = '$' : x
showPerlVars (VarSub x) = '&' : x

showPerlAST :: PerlAST -> String
showPerlAST (PerlInt n) = show n
showPerlAST (PerlStr x) = show x -- This is, though, Haskell's literal
showPerlAST (PerlVar t) = showPerlVars t
showPerlAST (PerlImplicitItem n) = "$_[" ++ show n ++ "]"
showPerlAST (PerlDeclare v t) = "my " ++ (showPerlVars v) ++
                                   " = (" ++ showPerlAST t ++ ")"
showPerlAST (PerlSubDeclare (VarSub s) (PerlAbstract t)) =
  "sub " ++ s ++ " { " ++ showPerlAST t ++ " }\n"
showPerlAST (PerlOp op t1 t2) = "(" ++ showPerlAST t1 ++ " "
                                ++ symbol op ++ " " ++
                                showPerlAST t2 ++ ")"
showPerlAST (PerlAbstract t) = "sub {" ++ " " ++ showPerlAST t ++ " }"
showPerlAST (PerlApp t1 ts) =
  "(" ++ showPerlAST t1 ++ ")->(" ++ terms ++ ")"
  where
    terms = concatMap (\(t, c) -> c ++ showPerlAST t)
                      (zip ts ("":repeat ", "))
showPerlAST (PerlSeq t1 t2) = showPerlAST t1 ++ "; " ++ showPerlAST t2
