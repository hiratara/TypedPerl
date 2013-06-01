import Control.Monad

data Info
instance Show Info where show = const "Info"
dummyInfo = undefined :: Info

data Term = TmTrue Info
          | TmFalse Info
          | TmIf Info Term Term Term
          | TmZero Info
          | TmSucc Info Term
          | TmPred Info Term
          | TmIsZero Info Term
          deriving Show

isNumericVal :: Term -> Bool
isNumericVal (TmZero _) = True
isNumericVal (TmSucc _ t) = isNumericVal t
isNumericVal _ = False

isVal :: Term -> Bool
isVal (TmTrue _) = True
isVal (TmFalse _) = True
isVal t | isNumericVal t = True
        | otherwise = False

eval1 :: Term -> Maybe Term
eval1 (TmIf _ (TmTrue _) t _) = Just t
eval1 (TmIf _ (TmFalse _) _ t) = Just t
eval1 (TmIf info t1 t2 t3) = (\t1' -> TmIf info t1' t2 t3) `fmap` eval1 t1
eval1 (TmSucc info t) = (\t' -> TmSucc info t') `fmap` eval1 t
eval1 (TmPred _ (TmZero _)) = Just (TmZero dummyInfo)
eval1 (TmPred _ (TmSucc _ v)) | isNumericVal v = Just v
eval1 (TmPred info t) = (\t' -> TmPred info t') `fmap` eval1 t
eval1 (TmIsZero _ (TmZero _)) = Just (TmTrue dummyInfo)
eval1 (TmIsZero _ (TmSucc _ v)) | isNumericVal v = Just (TmFalse dummyInfo)
eval1 (TmIsZero info t) = (\t' -> TmIsZero info t') `fmap` eval1 t
eval1 _ = Nothing

eval :: Term -> Term
eval t = maybe t eval (eval1 t)
