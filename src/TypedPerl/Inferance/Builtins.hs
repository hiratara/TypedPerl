module TypedPerl.Inferance.Builtins (
  initialTypeContext
  ) where
import qualified Data.Map as M
import TypedPerl.Inferance.TypeContext
import TypedPerl.Types

builtinContext :: Context
builtinContext = [
  (VarSub "print", asCTypeSchema [] printType)
  ]
  where
    printType = TypeArrow (TypeArg (RecNamed "a1" M.empty))
                          (TypeBuiltin TypeInt)

typeNames :: TypeNames
typeNames = map (('a' :) . show) [(1 :: Integer)..]

initialTypeContext :: TypeContext
initialTypeContext = TypeContext {names = typeNames, context = builtinContext}
