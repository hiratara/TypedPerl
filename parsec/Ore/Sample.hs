module Ore.Sample where
import Ore.Types
import Ore.Parsec
import Ore.Inferance

sample :: String
sample = "sub { $_[0] + 1 }"

showTerm :: String -> String
showTerm input = case parsePerl input of
  Left e -> show e
  Right ast -> showPerlAST ast ++ "\n"
               ++ (show . infer $ ast)

{-
*Perlsec> ((either show showPerlAST) . parsePerl) sample
"sub { ($_[0] + 1) }"
-}
