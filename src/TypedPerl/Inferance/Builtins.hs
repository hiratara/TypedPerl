module TypedPerl.Inferance.Builtins (
  initialTypeContext
  ) where
import qualified Data.Map as M
import TypedPerl.Inferance.TypeContext
import TypedPerl.Types

builtinCVar :: PerlVars -> PerlCVar
builtinCVar v = PerlCVar (NsGlobal "CORE") v

builtinContext :: Context
builtinContext = [
  (builtinCVar (VarSub "print"), asCTypeSchema [] printType)
  ]
  where
    printType = TypeArrow (TypeArg (RecNamed "a1" M.empty)) TypeUnknown

typeNames :: TypeNames
typeNames = map (('a' :) . show) [(1 :: Integer)..]

initialTypeContext :: TypeContext
initialTypeContext = TypeContext {
  names = typeNames
  , curPackage = "main"
  , context = builtinContext
  }
