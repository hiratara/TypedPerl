module Chap10 where

type Context = [(String, Binding)]
data Binding = NameBind | VarBind Ty deriving Show

data Ty = TyArr Ty Ty | TyBool deriving (Show, Eq)
data Term =
  TmVar Int Int
  | TmAbs String Ty Term
  | TmApp Term Term
  | TmTrue | TmFalse
  | TmIf Term Term Term
  deriving Show

indexToName :: Context -> Int -> String
indexToName ctx n = fst . (ctx !!) $ n

addBinding :: Context -> String -> Binding -> Context
addBinding ctx str b = (str, b) : ctx

getBinding :: Context -> Int -> Binding
getBinding ctx i = snd $ ctx !! i

getTypeFromContext :: Context -> Int -> Maybe Ty
getTypeFromContext ctx i = case binding' of
  (VarBind ty) -> Just ty
  _            -> Nothing
  where
    binding' = getBinding ctx i

typeOf :: Context -> Term -> Either String Ty
typeOf ctx  (TmVar i _) = do
  let ty = getTypeFromContext ctx i
  case ty of
    Just ty' -> return ty'
    _ -> Left "No type found in context"

typeOf ctx (TmAbs name ty t) = do
  let ctx' = addBinding ctx name (VarBind ty)
  ty' <- typeOf ctx' t
  return (TyArr ty ty')

typeOf ctx (TmApp t1 t2) = do
  ty1 <- typeOf ctx t1
  ty2 <- typeOf ctx t2
  case ty1 of
    TyArr ty2' ty3 | ty2 == ty2' -> return ty3
    _ -> Left "type error"

typeOf _ TmTrue = return TyBool

typeOf _ TmFalse = return TyBool

typeOf ctx (TmIf t1 t2 t3) = do
  t1' <- typeOf ctx t1
  case t1' of
    TyBool -> do
      t2' <- typeOf ctx t2
      t3' <- typeOf ctx t3
      if t2' == t3' 
        then return t2'
        else Left "types are not corrected"
    _ -> Left "not bool"


sample1 = TmTrue
sample2 = TmAbs "x" TyBool (sample1)
sample3 = TmApp (TmAbs "x" TyBool (TmVar 0 0)) TmFalse
sample4 = TmIf (TmAbs "x" TyBool (TmVar 0 0)) TmFalse TmTrue
sample5 = TmIf TmTrue sample3 TmTrue
sample6 = TmIf TmTrue sample2 TmTrue
