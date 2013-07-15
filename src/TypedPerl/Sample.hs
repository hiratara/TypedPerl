module TypedPerl.Sample where
import TypedPerl.Types
import TypedPerl.Parsec
import TypedPerl.Inferance

sample :: String
sample = "sub { $_[0] + 1 }"

showTerm :: String -> String
showTerm input = either id id input'
  where
    input' :: Either String String
    input' = do
      ast <- (leftMap show . parsePerl) input
      ty <- infer ast
      return (showPerlAST ast ++ "\n" ++ (showPerlType ty))

leftMap :: (a -> a') -> Either a b -> Either a' b
leftMap f (Left x)  = (Left . f) x
leftMap _ (Right y) = Right y

{-
*Perlsec> ((either show showPerlAST) . parsePerl) sample
"sub { ($_[0] + 1) }"
-}
