module Ore.Parsec where
import Control.Monad
import Data.Char
import Text.Parsec
import Ore.Builtins
import Ore.Types
import Debug.Trace (traceShow)

type PerlParser = Parsec String PerlState PerlAST

data PerlState = PerlState

parserBlock :: PerlParser
parserBlock = do
  t1 <- parserTerm
  let next = do
        eol
        t2 <- parserBlock
        return (PerlSeq t1 t2)
  try next <|> (optional eol >> return t1)
  where
    eol = (char ';' >> spaces) <|> eof

parserTerminalTerm :: PerlParser
parserTerminalTerm = do
  ret <- parserSub <|> parserMy <|>
         (try parserImplicitVar <|> parserVars) <|>
         parserInt <|> parserStr <|>
         between (char '(') (char ')') parserTerm
  spaces
  return ret

parserTerm :: PerlParser
parserTerm = do
  term <- try parserCallSub <|> try parserOp <|> parserTerminalTerm
  spaces
  return term

parserMy :: PerlParser
parserMy = do
  string "my" >> spaces
  (PerlVar v) <- parserVars
  spaces
  char '=' >> spaces
  t <- parserTerminalTerm
  return (PerlDeclare v t)

parserOp :: PerlParser
parserOp = do
  t1 <- parserTerminalTerm
  op <- choice (map parseOpSymbol builtinBinops)
  spaces
  t2 <- parserTerm
  return (PerlOp op t1 t2)
  where
    parseOpSymbol b = (string . symbol) b >> return b

parserImplicitVar :: PerlParser
parserImplicitVar = do
  string "$_[0]"
  return (PerlVar VarSubImplicit)

alphabetChars :: String
alphabetChars = '_' : [toEnum (fromEnum 'a' + n) | n <- [1 .. 26]]

digitChars :: String
digitChars = map (head . show) [(0 :: Int) .. 9]

parserVars :: PerlParser
parserVars = do
  char '$'
  n <- oneOf alphabetChars
  ns <- many (oneOf (alphabetChars ++ digitChars))
  return (PerlVar (VarNamed (n:ns)))

parserInt :: PerlParser
parserInt = do
  digits <- many1 digit
  let n = foldl (\x d -> 10 * x + toInteger (digitToInt d)) 0 digits
  return (PerlInt n)

parserStr :: PerlParser
parserStr = do
  char '"'
  str <- many (noneOf "\"") -- TODO
  char '"'
  return (PerlStr str)

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
