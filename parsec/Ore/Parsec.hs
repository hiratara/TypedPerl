module Ore.Parsec where
import Control.Monad
import Data.Char
import Text.Parsec
import Ore.Builtins
import Ore.Types
import Debug.Trace (traceShow)

type PerlParser = Parsec String PerlState PerlAST

data PerlState = PerlState

parserTopSequences :: PerlParser
parserTopSequences = do
  (t1, next) <- (do t <- try parserSubDeclare
                    return (t, True)
                ) <|> (
                 do t' <- parserTerm
                    m <- optionMaybe eol
                    return (t', maybe False (const True) m)
                )
  if next
    then try (parserTopSequences >>=
              return . (PerlSeq t1)) <|> return t1
    else return t1
  where
    eol = (char ';' >> spaces) <|> eof

parserSequences :: PerlParser
parserSequences = do
  t1 <- parserTerm
  let next = do
        eol
        t2 <- parserSequences
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
  term <- try parserCallSub <|> try parserCallAnonymous <|>
          try parserOp <|> parserTerminalTerm
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

perlSymbol :: Parsec String PerlState String
perlSymbol = many (oneOf (alphabetChars ++ digitChars))

parserVars :: PerlParser
parserVars = do
  char '$'
  n <- oneOf alphabetChars
  ns <- perlSymbol
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

parserBlock :: PerlParser
parserBlock = do
  char '{' >> spaces
  content <- parserSequences
  char '}' >> spaces
  return content

parserSubDeclare :: PerlParser
parserSubDeclare = do
  string "sub" >> space >> spaces
  sym <- perlSymbol
  spaces
  content <- parserBlock
  spaces
  return (PerlSubDeclare (VarSub sym) (PerlAbstract content))

parserSub :: PerlParser
parserSub = do
  string "sub" >> spaces
  PerlAbstract `fmap` parserBlock

parserCallSub :: PerlParser
parserCallSub = do
  sym <- perlSymbol
  spaces
  char '(' >> spaces
  t1 <- parserTerm
  char ')' >> spaces
  return (PerlApp (PerlVar (VarSub sym)) t1)

parserCallAnonymous :: PerlParser
parserCallAnonymous = do
  t1 <- parserTerminalTerm
  string "->" >> spaces >> char '(' >> spaces
  t2 <- parserTerm
  char ')'
  return (PerlApp t1 t2)

perlParser :: PerlParser
perlParser = parserTopSequences

parsePerl :: String -> Either ParseError PerlAST
parsePerl source = runParser perlParser PerlState [] source
