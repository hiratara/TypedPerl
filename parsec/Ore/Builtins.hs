module Ore.Builtins (
  builtinBinops
  ) where
import Ore.Types

builtinBinops :: [PerlBinOp]
builtinBinops = [
  PerlBinOp "+" (TypeBuiltin TypeInt) (TypeBuiltin TypeInt)
                (TypeBuiltin TypeInt)
  , PerlBinOp "-" (TypeBuiltin TypeInt) (TypeBuiltin TypeInt)
                  (TypeBuiltin TypeInt)
  , PerlBinOp "*" (TypeBuiltin TypeInt) (TypeBuiltin TypeInt)
                  (TypeBuiltin TypeInt)
  , PerlBinOp "/" (TypeBuiltin TypeInt) (TypeBuiltin TypeInt)
                  (TypeBuiltin TypeInt)
  , PerlBinOp "." (TypeBuiltin TypeStr) (TypeBuiltin TypeStr)
                  (TypeBuiltin TypeStr)
  , PerlBinOp "x" (TypeBuiltin TypeStr) (TypeBuiltin TypeInt)
                  (TypeBuiltin TypeStr)
  ]
