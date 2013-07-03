module Perlsec where
import Data.Char
import Text.Parsec
import Debug.Trace (traceShow)

type PerlParser = Parsec String PerlState PerlAST

data PerlType =
  TypeUnknown
  | TypeInt
  | TypeArrow PerlType PerlType
  deriving (Show, Eq)

data PerlVars =
  VarSubImplicit
  deriving Show

data PerlAST =
  PerlInt Integer
  | PerlVar PerlVars PerlType
  | PerlOp String PerlAST PerlAST
  | PerlAbstract PerlAST
  | PerlApp PerlAST PerlAST
  deriving Show

data PerlState = PerlState

showPerlVars :: PerlVars -> String
showPerlVars VarSubImplicit = "$_[0]"

showPerlAST :: PerlAST -> String
showPerlAST (PerlInt n) = show n
showPerlAST (PerlVar t _) = showPerlVars t
showPerlAST (PerlOp op t1 t2) = "(" ++ showPerlAST t1 ++ " "
                                ++ op ++ " " ++
                                showPerlAST t2 ++ ")"
showPerlAST (PerlAbstract t) = "sub {" ++ " " ++ showPerlAST t ++ " }"
showPerlAST (PerlApp t1 t2) = "(" ++ showPerlAST t1 ++
                              ")->(" ++ showPerlAST t2 ++ ")"

parserTerminalTerm :: PerlParser
parserTerminalTerm = do
  ret <- parserSub <|> parserImplicitVar <|> parserInt
  spaces
  return ret

parserTerm :: PerlParser
parserTerm = do
  term <- try parserCallSub <|> try parserOp <|> parserTerminalTerm
  spaces
  return term

parserOp :: PerlParser
parserOp = do
  t1 <- parserTerminalTerm
  op <- oneOf "+-*/"
  spaces
  t2 <- parserTerm
  return (PerlOp (op:"") t1 t2)

parserImplicitVar :: PerlParser
parserImplicitVar = do
  string "$_[0]"
  return (PerlVar VarSubImplicit TypeUnknown)

parserInt :: PerlParser
parserInt = do
  digits <- many1 digit
  let n = foldl (\x d -> 10 * x + toInteger (digitToInt d)) 0 digits
  return (PerlInt n)

parserSub :: PerlParser
parserSub = do
  string "sub" >> spaces >> char '{' >> spaces
  content <- parserTerm
  char '}'
  return (PerlAbstract content)

parserCallSub :: PerlParser
parserCallSub = do
  t1 <- parserTerminalTerm
  string "->" >> spaces >> char '(' >> spaces
  t2 <- parserTerm
  char ')'
  return (PerlApp t1 t2)

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
