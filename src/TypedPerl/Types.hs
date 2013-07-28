module TypedPerl.Types (
  PerlTypeVars (..),
  RecsVar,
  PerlRecs (..),
  PerlTypeBuiltins (..),
  PerlType (..),
  PerlVars (..),
  PerlBinOp (..),
  PerlAST (..),
  showPerlVars, showPerlAST,
  showPerlTypeVars, showPerlType
  ) where
import qualified Data.Map as M

data PerlTypeVars =
  TypeNamed String
  deriving (Show, Eq)

type RecsVar = String
data PerlRecs k =
  RecEmpty (M.Map k PerlType)
  | RecNamed RecsVar (M.Map k PerlType)
  deriving (Show, Eq)

data PerlTypeBuiltins =
  TypeStr
  | TypeInt
  deriving (Show, Eq)

data PerlType =
  TypeVar PerlTypeVars
  | TypeUnknown
  | TypeBuiltin PerlTypeBuiltins
  | TypeArg (PerlRecs Int)
  | TypeObj (PerlRecs String)
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
  | PerlImplicitItem PerlAST Int
  | PerlOp PerlBinOp PerlAST PerlAST
  | PerlObj (M.Map String PerlAST) String
  | PerlObjItem PerlAST String
  | PerlAbstract PerlAST
  | PerlApp PerlAST [PerlAST]
  | PerlSeq PerlAST PerlAST
  deriving (Show, Eq)

showPerlTypeVars :: PerlTypeVars -> String
showPerlTypeVars (TypeNamed x) = x

showRecsMap :: Show k => M.Map k PerlType -> String
showRecsMap m = "{" ++ content ++ "}"
  where
    content = concat $ map (\(k, v) -> show k ++ ":" ++ showPerlType v)
                           (M.assocs m)

showPerlRecs :: Show k => PerlRecs k -> String
showPerlRecs (RecEmpty m) = showRecsMap m -- TODO
showPerlRecs (RecNamed v m) = v ++ "⊕" ++ showRecsMap m

showPerlTypeBuiltins :: PerlTypeBuiltins -> String
showPerlTypeBuiltins TypeInt = "Int"
showPerlTypeBuiltins TypeStr = "Str"

showPerlType :: PerlType -> String
showPerlType (TypeVar tyv) = showPerlTypeVars tyv
showPerlType TypeUnknown = "?"
showPerlType (TypeBuiltin ty) = showPerlTypeBuiltins ty
showPerlType (TypeArrow ty1 ty2) = '(' : showPerlType ty1 ++ ") -> ("
                                   ++ showPerlType ty2 ++ ")"
showPerlType (TypeArg r) = showPerlRecs r
showPerlType (TypeObj r) = showPerlRecs r

showPerlVars :: PerlVars -> String
showPerlVars VarSubImplicit = "@_"
showPerlVars (VarNamed x) = '$' : x
showPerlVars (VarSub x) = '&' : x

showPerlAST :: PerlAST -> String
showPerlAST (PerlInt n) = show n
showPerlAST (PerlStr x) = show x -- This is, though, Haskell's literal
showPerlAST (PerlVar t) = showPerlVars t
showPerlAST (PerlImplicitItem _ n) = "$_[" ++ show n ++ "]"
showPerlAST (PerlDeclare v t) = "my " ++ (showPerlVars v) ++
                                   " = (" ++ showPerlAST t ++ ")"
showPerlAST (PerlSubDeclare (VarSub s) (PerlAbstract t)) =
  "sub " ++ s ++ " { " ++ showPerlAST t ++ " }\n"
showPerlAST (PerlOp op t1 t2) = "(" ++ showPerlAST t1 ++ " "
                                ++ symbol op ++ " " ++
                                showPerlAST t2 ++ ")"
showPerlAST (PerlObj m x) = "bless " ++ hash ++ ", \"" ++ x ++ "\""
  where
    hash = "{" ++ hashContent ++ "}"
    hashContent = concat $ map (\(k, v) -> k ++ " => " ++ showPerlAST v)
                               (M.assocs m)
showPerlAST (PerlObjItem t x) = "(" ++ showPerlAST t ++ ")->{" ++ x ++ "}"
showPerlAST (PerlAbstract t) = "sub {" ++ " " ++ showPerlAST t ++ " }"
showPerlAST (PerlApp t1 ts) =
  "(" ++ showPerlAST t1 ++ ")->(" ++ terms ++ ")"
  where
    terms = concatMap (\(t, c) -> c ++ showPerlAST t)
                      (zip ts ("":repeat ", "))
showPerlAST (PerlSeq t1 t2) = showPerlAST t1 ++ "; " ++ showPerlAST t2
