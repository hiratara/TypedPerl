module Chap25 where

data Ty = 
  TyVar Int Int
  | TyArr Ty Ty
  | TyAll String Ty
  | TySome String Ty
  deriving (Show, Eq)

data Binding =
  NameBind
  | VarBind Ty
  | TyVarBind
  deriving Show

addBinding :: Context -> String -> Binding -> Context
addBinding ctx str b = (str, b) : ctx

tyMap :: (Int -> Int -> Int -> Ty) -> Int -> Ty -> Ty
tyMap onvar c ty = walk c ty
  where
    walk c' (TyVar x n) = onvar c' x n
    walk c' (TyArr ty1 ty2) = TyArr (walk c' ty1) (walk c' ty2)
    walk c' (TyAll x ty') = TyAll x (walk (c' + 1) ty')
    walk c' (TySome x ty') = TySome x (walk (c' + 1) ty')

typeShiftAbove :: Int -> Int -> Ty -> Ty
typeShiftAbove d c ty = tyMap onvar c ty
  where onvar c' x n
          | x >= c' = TyVar (x + d) (n + d)
          | otherwise = TyVar x (n + d)

typeShift :: Int -> Ty -> Ty
typeShift d ty = typeShiftAbove d 0 ty

typeSubst :: Int -> Ty -> Ty -> Ty
typeSubst i ty1 ty2 = tyMap onvar i ty2
  where
    onvar j x n
      | x == j = typeShift j ty1
      | otherwise = TyVar x n

typeSubstTop :: Ty -> Ty -> Ty
typeSubstTop ty1 ty2 =
  typeShift (-1) (typeSubst 0 (typeShift 1 ty1) ty2)

type Context = [(String, Binding)]
data Term =
  TmVar Int Int
  | TmAbs String Ty Term
  | TmApp Term Term
  | TmTAbs String Term
  | TmTApp Term Ty
  | TmPack Ty Term Ty
  | TmUnpack String String Term Term
  deriving Show

tmMap :: (Int -> Int -> Int -> Term) ->
         (Int -> Ty -> Ty) ->
         Int -> Term -> Term
tmMap onvar ontype c t = walk c t
  where
    walk c' (TmVar x n) = onvar c' x n
    walk c' (TmAbs x ty t') = TmAbs x (ontype c' ty) (walk (c' + 1) t')
    walk c' (TmApp t1 t2) = TmApp (walk c' t1) (walk c' t2)
    walk c' (TmTAbs tyX t') = TmTAbs tyX (walk (c' + 1) t')
    walk c' (TmTApp t' ty) = TmTApp (walk c' t') (ontype c' ty)
    walk c' (TmPack ty1 t' ty2) = TmPack (ontype c' ty1) (walk c' t')
                                         (ontype c' ty2)
    walk c' (TmUnpack tyX x t1 t2) =
      TmUnpack tyX x (walk c' t1) (walk (c' + 2) t2)

termShiftAbove :: Int -> Int -> Term -> Term
termShiftAbove d c t = tmMap onvar ontype c t
  where
    onvar c' x n
      | x >= c' = TmVar (x + d) (n + d)
      | otherwise = TmVar x (n + d)
    ontype = typeShiftAbove d

termShift :: Int -> Term -> Term
termShift d = termShiftAbove d 0

termSubst :: Int -> Term -> Term -> Term
termSubst j t1 t2 = tmMap onvar ontype j t2
  where
    onvar j' x n
      | j' == x = termShift j' t1
      | otherwise = TmVar x n
    ontype _ ty = ty

termSubstTop :: Term -> Term -> Term
termSubstTop t1 t2 = termShift (-1) (termSubst 0 (termShift 1 t1) t2)

tytermSubst :: Int -> Ty -> Term -> Term
tytermSubst i ty t = tmMap onvar ontype i t
  where
    onvar _ x n = TmVar x n
    ontype j ty' = typeSubst j ty ty'

tytermSubstTop :: Ty -> Term -> Term
tytermSubstTop ty t = termShift (-1) (tytermSubst 0 (typeShift 1 ty) t)

isVal :: Context -> Term -> Bool
isVal _ (TmAbs _ _ _) = True
isVal _ _ = False

eval1 :: Context -> Term -> Either String Term
eval1 ctx (TmApp (TmAbs _ _ t1) v2)
  | isVal ctx v2 = return (termSubstTop v2 t1)
eval1 ctx (TmApp v1 t2)
  | isVal ctx v1 = do
    t2' <- eval1 ctx t2
    return (TmApp v1 t2')
eval1 ctx (TmApp t1 t2) = do
  t1' <- eval1 ctx t1
  return (TmApp t1' t2)
eval1 _ (TmTApp (TmTAbs _ t) ty) = return (tytermSubstTop ty t)
eval1 ctx (TmTApp t ty) = do
  t' <- eval1 ctx t
  return (TmTApp t' ty)
eval1 ctx (TmUnpack  _ _ (TmPack ty1 v _) t) 
  | isVal ctx v = return (tytermSubstTop ty1 t')
  where t' = termSubstTop (termShift 1 v) t
eval1 ctx (TmUnpack tyX x t1 t2) = do
  t1' <- eval1 ctx t1
  return (TmUnpack tyX x t1' t2)
eval1 ctx (TmPack ty t ty') = do
  t' <- eval1 ctx t
  return (TmPack ty t' ty')
eval1 _ _ = Left "NoRuleApplies"

eval :: Context -> Term -> Term
eval ctx t = either (const t) (eval ctx) (eval1 ctx t)

typeOf :: Context -> Term -> Either String Ty
typeOf ctx (TmAbs x ty t) = do
  let ctx' = addBinding ctx x (VarBind ty)
  tyT <- typeOf ctx' t
  return (TyArr ty tyT)
typeOf ctx (TmApp t1 t2) = do
  tyT1 <- typeOf ctx t1
  tyT2 <- typeOf ctx t2
  case tyT1 of
    TyArr ty' _ | ty' == tyT2 -> undefined
    _ -> Left "Not Arrow"
typeOf ctx (TmTAbs tyX t) = do
  let ctx' = addBinding ctx tyX TyVarBind
  ty' <- typeOf ctx' t
  return (TyAll tyX ty')
typeOf ctx (TmTApp t ty) = do
  tyT <- typeOf ctx t
  case tyT of
    TyAll _ ty' -> return (typeSubstTop ty ty')
    _ -> undefined
typeOf ctx (TmPack ty1 t ty2) =
  case ty2 of
    TySome _ ty' -> do
      tyT <- typeOf ctx t
      let ty'' = typeSubstTop ty1 ty'
      if tyT == ty''
        then return ty2
        else Left "doesn't match declared type"
    _ -> Left "no existensial types"
typeOf ctx (TmUnpack tyX x t1 t2) = do
  tyT1 <- typeOf ctx t1
  case tyT1 of
    TySome _ ty' -> do
      let ctx' = addBinding' x (VarBind ty') .
                 addBinding' tyX TyVarBind $ ctx
      tyT2 <- typeOf ctx' t2
      return (typeShift (-2) tyT2)
    _ -> Left ""
  where
    addBinding' x0 b0 ctx0 = addBinding ctx0 x0 b0
