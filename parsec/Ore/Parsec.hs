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
                 do t' <- parserSentence
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
  t1 <- parserSentence
  let next = do
        eol
        t2 <- parserSequences
        return (PerlSeq t1 t2)
  try next <|> (optional eol >> return t1)
  where
    eol = (char ';' >> spaces) <|> eof

parserSentence :: PerlParser
parserSentence = do
  ast <- parserMy <|> parserTerm
  spaces
  return ast

parserTerm :: PerlParser
parserTerm = do
  t <- parserCallAnonymous
  spaces
  parserTerm' t

parserTerm' :: PerlAST -> PerlParser
parserTerm' t1 =
  (try $ do
      op <- choice (map parseOpSymbol builtinBinops)
      spaces
      t2 <- parserCallAnonymous
      spaces
      parserTerm' (PerlOp op t1 t2)
  ) <|> return t1
  where
    parseOpSymbol b = (string . symbol) b >> return b

parserCallAnonymous :: PerlParser
parserCallAnonymous = do
  atom <- parserAtom
  parserCallAnonymous' atom

parserCallAnonymous' :: PerlAST -> PerlParser
parserCallAnonymous' callie =
  (try $ do
    string "->" >> spaces >> char '(' >> spaces
    t <- parserTerm
    char ')'
    parserCallAnonymous' (PerlApp callie t)
  ) <|> return callie

parserAtom :: PerlParser
parserAtom = do
  ast <- parserInt <|> parserStr <|>
         between (char '(') (char ')') parserTerm <|>
         parserSub <|> parserCallSub <|>
         try parserImplicitVar <|> parserVars
  spaces
  return ast

parserMy :: PerlParser
parserMy = do
  string "my" >> spaces
  (PerlVar v) <- parserVars
  spaces
  char '=' >> spaces
  t <- parserTerm
  return (PerlDeclare v t)

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

perlParser :: PerlParser
perlParser = parserTopSequences

parsePerl :: String -> Either ParseError PerlAST
parsePerl source = runParser perlParser PerlState [] source
