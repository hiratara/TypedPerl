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
parserTerm = precedence24

precedence24 :: PerlParser
precedence24 = precedence23

precedence23 :: PerlParser
precedence23 = precedence22

precedence22 :: PerlParser
precedence22 = precedence21

precedence21 :: PerlParser
precedence21 = precedence20

precedence20 :: PerlParser
precedence20 = precedence19

precedence19 :: PerlParser
precedence19 = precedence18

precedence18 :: PerlParser
precedence18 = precedence17

precedence17 :: PerlParser
precedence17 = precedence16

precedence16 :: PerlParser
precedence16 = precedence15

precedence15 :: PerlParser
precedence15 = precedence14

precedence14 :: PerlParser
precedence14 = precedence13

precedence13 :: PerlParser
precedence13 = precedence12

precedence12 :: PerlParser
precedence12 = precedence11

precedence11 :: PerlParser
precedence11 = precedence10

precedence10 :: PerlParser
precedence10 = precedence9

precedence9 :: PerlParser
precedence9 = precedence8

precedence8 :: PerlParser
precedence8 = parserBinOp precedence7 (map (: []) "+-.")

precedence7 :: PerlParser
precedence7 = parserBinOp precedence6 (map (: []) "*/%x")

precedence6 :: PerlParser
precedence6 = precedence5

precedence5 :: PerlParser
precedence5 = precedence4

precedence4 :: PerlParser
precedence4 = precedence3

precedence3 :: PerlParser
precedence3 = precedence2

precedence2 :: PerlParser
precedence2 = do
  atom <- precedence1
  precedence2' atom
  where
    precedence2' :: PerlAST -> PerlParser
    precedence2' callie =
      (try $ do
        string "->" >> spaces
        ts <- parserArgs
        precedence2' (PerlApp callie ts)
      ) <|> return callie

precedence1 :: PerlParser
precedence1 = do
  ast <- parserInt <|> parserStr <|>
         between (char '(') (char ')') parserTerm <|>
         try parserCallSub <|>
         try parserImplicitVar <|> parserVars <|>
         parserSub -- Not specified in perlop.pod
  spaces
  return ast

parserBinOp :: PerlParser -> [String] -> PerlParser
parserBinOp operandParser symbols = do
  t <- operandParser
  spaces
  parserBinOp' t
  where
    parserBinOp' :: PerlAST -> PerlParser
    parserBinOp' t1 =
      (try $ do
        op <- choice (map string symbols)
        spaces
        t2 <- operandParser
        spaces
        parserBinOp' (PerlOp (lookupOp builtinBinops op) t1 t2)
      ) <|> return t1
      where
        lookupOp [] _ = error "[BUG]but builtin operators"
        lookupOp (x:xs) sym
          | symbol x == sym = x
          | otherwise = lookupOp xs sym

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
  string "$_[" >> spaces
  c <- many1 digit
  spaces >> char ']'
  return (PerlImplicitItem (read c))

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
  ts <- parserArgs
  return (PerlApp (PerlVar (VarSub sym)) ts)

parserArgs :: Parsec String PerlState [PerlAST]
parserArgs = do
  char '(' >> spaces
  ts <- parserTerm `sepBy` (char ',' >> spaces)
  char ')' >> spaces
  return ts

perlParser :: PerlParser
perlParser = parserTopSequences

parsePerl :: String -> Either ParseError PerlAST
parsePerl source = runParser perlParser PerlState [] source
