module Perlsec where
import Data.Char
import Text.Parsec
import Debug.Trace (traceShow)

type PerlParser = Parsec String PerlState PerlAST

data PerlAST =
  PerlInt Integer
  | PerlImplicitVar
  | PerlOp String PerlAST PerlAST
  | PerlAbstract PerlAST
  deriving Show
data PerlState = PerlState

showPerlAST :: PerlAST -> String
showPerlAST (PerlInt n) = show n
showPerlAST PerlImplicitVar = "$_[0]"
showPerlAST (PerlOp op t1 t2) = "(" ++ showPerlAST t1 ++ " "
                                ++ op ++ " " ++
                                showPerlAST t2 ++ ")"
showPerlAST (PerlAbstract t) = "sub {" ++ " " ++ showPerlAST t ++ " }"

parserTerm :: PerlParser
parserTerm = do
  term <- parserSub <|> try parserOp <|> parserImplicitVar <|> parserInt
  spaces
  return term

parserOp :: PerlParser
parserOp = do
  t1 <- parserImplicitVar <|> parserInt
  spaces
  op <- oneOf "+-*/"
  spaces
  t2 <- parserImplicitVar <|> parserInt
  return (PerlOp (op:"") t1 t2)

parserImplicitVar :: PerlParser
parserImplicitVar = do
  string "$_[0]"
  return PerlImplicitVar

parserInt :: PerlParser
parserInt = do
  digits <- many1 digit
  spaces
  let n = foldl (\x d -> 10 * x + toInteger (digitToInt d)) 0 digits
  return (PerlInt n)

parserSub :: PerlParser
parserSub = do
  string "sub" >> spaces >> char '{' >> spaces
  content <- parserTerm
  char '}'
  return (PerlAbstract content)

perlParser :: PerlParser
perlParser = parserTerm

parsePerl :: String -> Either ParseError PerlAST
parsePerl source = runParser perlParser PerlState [] source

sample :: String
sample = "sub { $_[0] + 1 }"

{-
*Perlsec> ((either show showPerlAST) . parsePerl) sample
"sub { ($_[0] + 1) }"
-}
