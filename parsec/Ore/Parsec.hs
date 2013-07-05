module Ore.Parsec where
import Data.Char
import Text.Parsec
import Ore.Types
import Debug.Trace (traceShow)

type PerlParser = Parsec String PerlState PerlAST

data PerlState = PerlState

parserBlock :: PerlParser
parserBlock = do
  t1 <- parserTerm
  let next = do
        (char ';' >> spaces) <|> eof
        t2 <- parserBlock
        return (PerlSeq t1 t2)
  next <|> return t1

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
  return (PerlVar VarSubImplicit (TypeVar TypeUnknown))

parserInt :: PerlParser
parserInt = do
  digits <- many1 digit
  let n = foldl (\x d -> 10 * x + toInteger (digitToInt d)) 0 digits
  return (PerlInt n)

parserSub :: PerlParser
parserSub = do
  string "sub" >> spaces >> char '{' >> spaces
  content <- parserBlock
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
perlParser = parserBlock

parsePerl :: String -> Either ParseError PerlAST
parsePerl source = runParser perlParser PerlState [] source
