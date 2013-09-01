module TypedPerl.Types (
  PerlTypeVars (..),
  RecsVar,
  PerlRecs (..),
  PerlTypeBuiltins (..),
  PerlType (..),
  PerlNamespace (..),
  PerlVars (..),
  PerlBinOp (..),
  PerlAST (..),
  showPerlVars, showPerlAST,
  showPerlTypeVars, showPerlType, showPerlRecs
  ) where
import Data.List
import qualified Data.Map as M

data PerlTypeVars =
  TypeNamed String
  deriving (Show, Eq, Ord)

type RecsVar = String
data PerlRecs k =
  RecEmpty {recMap :: M.Map k PerlType}
  | RecNamed {recName :: RecsVar, recMap :: M.Map k PerlType}
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
  | TypeObj (PerlRecs String) (PerlRecs String)
  | TypeArrow PerlType PerlType
  | TypeFix PerlTypeVars PerlType
  deriving (Show, Eq)

data PerlNamespace = NsLexical | NsGlobal String deriving (Show, Eq)

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
  PerlDeclare PerlVars PerlAST
  | PerlInt Integer
  | PerlStr String
  | PerlVar PerlVars
  | PerlImplicitItem PerlAST Int
  | PerlOp PerlBinOp PerlAST PerlAST
  | PerlObj (M.Map String PerlAST) String
  | PerlObjItem PerlAST String
  | PerlObjMeth PerlAST String [PerlAST]
  | PerlAbstract PerlAST
  | PerlApp PerlAST [PerlAST]
  | PerlSeq PerlAST PerlAST
  | PerlPackage String PerlAST
  deriving (Show, Eq)

showPerlTypeVars :: PerlTypeVars -> String
showPerlTypeVars (TypeNamed x) = x

showRecsMap :: Show k => M.Map k PerlType -> String
showRecsMap m = "{" ++ content ++ "}"
  where
    content = intercalate ", " $ map (\(k, v) -> show k ++ ":" ++ showPerlType v)
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
showPerlType (TypeFix tyv ty) = "μ" ++ showPerlTypeVars tyv ++ ".(" ++ showPerlType ty ++ ")"
showPerlType (TypeArg r) = showPerlRecs r
showPerlType (TypeObj fi me) = "{fields => " ++ showPerlRecs fi
                               ++ ", methods => " ++ showPerlRecs me ++ "}"

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
showPerlAST (PerlOp op t1 t2) = "(" ++ showPerlAST t1 ++ " "
                                ++ symbol op ++ " " ++
                                showPerlAST t2 ++ ")"
showPerlAST (PerlObj m x) = "bless " ++ hash ++ ", \"" ++ x ++ "\""
  where
    hash = "{" ++ hashContent ++ "}"
    hashContent = concat $ map (\(k, v) -> k ++ " => " ++ showPerlAST v)
                               (M.assocs m)
showPerlAST (PerlObjItem t x) = "(" ++ showPerlAST t ++ ")->{" ++ x ++ "}"
showPerlAST (PerlObjMeth t x ts) = "(" ++ showPerlAST t ++ ")->" ++ x ++
                                   "(" ++ showTerms ts ++ ")"
showPerlAST (PerlAbstract t) = "sub {" ++ " " ++ showPerlAST t ++ " }"
showPerlAST (PerlApp t1 ts) =
  "(" ++ showPerlAST t1 ++ ")->(" ++ showTerms ts ++ ")"
showPerlAST (PerlSeq t1 t2) = showPerlAST t1 ++ "; " ++ showPerlAST t2
showPerlAST (PerlPackage name t) = "package " ++ name ++ ";\n" ++ showPerlAST t;

showTerms :: [PerlAST] -> String
showTerms ts = concatMap (\(t, c) -> c ++ showPerlAST t)
                         (zip ts ("":repeat ", "))
