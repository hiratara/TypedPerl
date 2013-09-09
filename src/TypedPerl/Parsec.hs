module TypedPerl.Parsec (
  parsePerl
  ) where
import Data.Char
import qualified Data.Map as M
import Data.List
import Text.Parsec
import TypedPerl.Builtins
import TypedPerl.Types

type PerlParserBase r = Parsec String PerlState r
type PerlParser = PerlParserBase PerlAST

data PerlState = PerlState

asAST :: PerlAST' -> Parsec s u PerlAST
asAST ast' = do
  s <- statePos `fmap` getParserState
  return (let info = SourceInfo (sourceName s) (sourceLine s) (sourceColumn s)
           in PerlAST info ast')

foldr1M :: Monad m => (a -> a -> m a) -> [a] -> m a
foldr1M f xs = foldr ((=<<) . f) (return x') xs'
  where sx = reverse xs
        x' = head sx
        xs' = (reverse . tail) sx

parserPackages :: PerlParser
parserPackages = do
  mainAST <- optionMaybe parserTopSequences
  asts <- many parserOnePackage
  eof
  let asts' = maybe asts (: asts) mainAST
  foldr1M ((asAST .) . PerlSeq) asts'
  where
    parserOnePackage =  do
      string "package" >> space >> spaces
      name <- perlClassname
      spaces
      char ';' >> spaces
      ast <- parserTopSequences
      asAST (PerlPackage name ast)

parserTopSequences :: PerlParser
parserTopSequences = do
  ts <- (many . try) (try (do {x <- parserSentence; eol; return x}) <|>
                           do {x <- parserSubDeclare; optional eol; return x})
  lastTerm <- optionMaybe (try parserSentence <|> parserSubDeclare)
  let ts' = maybe ts (\t -> ts ++ [t]) lastTerm
  if null ts' then parserFail "NO SENTENCES" else foldr1M ((asAST .) . PerlSeq) ts'
  where
    eol = many1 (char ';' >> spaces) >> return ()

parserSequences :: PerlParser
parserSequences = do
  t1 <- parserSentence
  let next = do
        eol
        t2 <- parserSequences
        asAST (PerlSeq t1 t2)
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
        (try $
          do
            char '{' >> spaces
            name <- perlSymbol
            char '}' >> spaces
            precedence2' =<< asAST (PerlObjItem callie name)
          ) <|> (
          do
            name <- perlSymbol
            ts <- parserArgs
            precedence2' =<< asAST (PerlObjMeth callie name ts)
          ) <|> (
          do
            ts <- parserArgs
            precedence2' =<< asAST (PerlApp callie ts)
          )
      ) <|> return callie

precedence1 :: PerlParser
precedence1 = do
  ast <- parserInt <|> parserStr <|>
         between (char '(') (char ')') parserTerm <|>
         try parserCallSub <|>
         try parserImplicitVar <|> (PerlVar `fmap` parserVars >>= asAST) <|>
         parserSub <|> -- Not specified in perlop.pod
         parserObj     -- Not specified in perlop.pod
  spaces
  return ast
  where
    parserObj = do
      string "bless" >> spaces
      m <- parserHashRef
      spaces
      char ',' >> spaces
      name <- between (char '"') (char '"') perlClassname
      asAST (PerlObj m name)
    parserHashRef = between (char '{') (char '}') content
    content = M.fromList `fmap` (pair `sepBy` (char ',' >> spaces))
    pair = do
      key <- perlSymbol
      spaces
      string "=>" >> spaces
      value <- parserTerm
      return (key, value)

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
        parserBinOp' =<< asAST (PerlOp (lookupOp builtinBinops op) t1 t2)
      ) <|> return t1
      where
        lookupOp [] _ = error "[BUG]but builtin operators"
        lookupOp (x:xs) sym
          | symbol x == sym = x
          | otherwise = lookupOp xs sym

parserMy :: PerlParser
parserMy = do
  string "my" >> spaces
  v <- parserVars
  spaces
  char '=' >> spaces
  t <- parserTerm
  asAST (PerlDeclare v t)

parserImplicitVar :: PerlParser
parserImplicitVar = do
  string "$_[" >> spaces
  c <- many1 digit
  spaces >> char ']'
  v <- asAST (PerlVar VarSubImplicit)
  asAST (PerlImplicitItem v (read c))

uAlphabetChars :: String
uAlphabetChars = '_' : [toEnum (fromEnum 'A' + n) | n <- [0 .. 25]]

alphabetChars :: String
alphabetChars = '_' : [toEnum (fromEnum 'a' + n) | n <- [0 .. 25]]

digitChars :: String
digitChars = map (head . show) [(0 :: Int) .. 9]

symbolChars :: String
symbolChars = uAlphabetChars ++ alphabetChars ++ digitChars

perlSymbol :: PerlParserBase String
perlSymbol = do
  c <- oneOf alphabetChars
  cs <- many (oneOf symbolChars)
  return (c:cs)

perlClassname :: PerlParserBase String
perlClassname = do
  first <- parts
  lefts <- many (do {string "::"; parts})
  return (intercalate "::" (first : lefts))
  where parts = do
          c <- oneOf (uAlphabetChars ++ alphabetChars)
          cs <- many (oneOf symbolChars)
          return (c:cs)

parserVars :: PerlParserBase PerlVars
parserVars = do
  char '$'
  sym <- perlSymbol
  return (VarNamed sym)

parserInt :: PerlParser
parserInt = do
  digits <- many1 digit
  let n = foldl (\x d -> 10 * x + toInteger (digitToInt d)) 0 digits
  asAST (PerlInt n)

parserStr :: PerlParser
parserStr = parserStr' >>= (asAST . PerlStr)

parserStr' :: PerlParserBase String
parserStr'  = do
  char '"'
  str <- many (noneOf "\"") -- TODO
  char '"'
  return str

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
  abst <- asAST (PerlAbstract content)
  asAST (PerlDeclare (VarSub sym) abst)

parserSub :: PerlParser
parserSub = do
  string "sub" >> spaces
  (asAST . PerlAbstract) =<< parserBlock

parserCallSub :: PerlParser
parserCallSub = do
  sym <- perlSymbol
  spaces
  ts <- parserArgs
  v <- asAST (PerlVar (VarSub sym))
  asAST (PerlApp v ts)

parserArgs :: PerlParserBase [PerlAST]
parserArgs = do
  char '(' >> spaces
  ts <- parserTerm `sepBy` (char ',' >> spaces)
  char ')' >> spaces
  return ts

perlParser :: PerlParser
perlParser = parserPackages

parsePerl :: String -> Either ParseError PerlAST
parsePerl source = runParser perlParser PerlState [] source
