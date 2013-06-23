module Chap7 where
data Term = TmVar Int Int | TmAbs String Term | TmApp Term Term deriving Show
type Context = [(String, Binding)]
data Binding = NameBind deriving Show

showTerm :: Context -> Term -> String
showTerm ctx (TmAbs x t) = "(lambda " ++ x' ++ ". " ++ showTerm ctx' t ++ ")"
  where
    (ctx', x') = pickFreshName ctx x

showTerm ctx (TmApp t1 t2) = "(" ++ showTerm ctx t1 ++ " " ++ showTerm ctx t2 ++ ")"
showTerm ctx (TmVar x n)
  | contextLength ctx == n = indexToName ctx x
  | otherwise = "[bad index]"

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx symbol = ((newSymbol, NameBind):ctx, newSymbol)
  where
    newSymbol = head $ dropWhile (\s -> s `elem` map fst ctx) symbols
    symbols = (map reverse) . scanl (flip (:)) symbol . repeat $ '\''

contextLength :: Context -> Int
contextLength = length

indexToName :: Context -> Int -> String
indexToName ctx n = fst . (ctx !!) $ n

termShift :: Int -> Term -> Term
termShift d t = walk 0 t
  where
    walk c (TmVar x n)
      | x >= c = TmVar (x + d) (n + d)
      | otherwise = TmVar x (n + d)
    walk c (TmAbs x t') = TmAbs x (walk (c + 1) t')
    walk c (TmApp t1 t2) = TmApp (walk c t1) (walk c t2)

termSubst :: Int -> Term -> Term -> Term
termSubst j s t = walk 0 t
  where
    walk c t'@(TmVar x _)
      | x == j + c = termShift c s
      | otherwise = t'
    walk c (TmAbs x t'') = TmAbs x (walk (c + 1) t'')
    walk c (TmApp t1 t2) = TmApp (walk c t1) (walk c t2)

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

isVal :: a -> Term -> Bool
isVal _ (TmAbs _ _) = True
isVal _ _ = False

eval1 :: a -> Term -> Either String Term
eval1 ctx (TmApp (TmAbs _ t1) v2)
  | isVal ctx v2 = return (termSubstTop v2 t1)
eval1 ctx (TmApp v1 t2)
  | isVal ctx v1 = do
    t2' <- eval1 ctx t2
    return (TmApp v1 t2')
eval1 ctx (TmApp t1 t2) = do
  t1' <- eval1 ctx t1
  return (TmApp t1' t2)
eval1 _ _ = Left "NoRuleApplies"

eval :: a -> Term -> Term
eval ctx t = either (const t) (eval ctx) (eval1 ctx t)
