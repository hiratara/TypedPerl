module Ore.Inferance (
  infer
  ) where
import Ore.Substitute
import Ore.Types
import Ore.Utils
import Control.Monad.State
import qualified Data.Map as M
import Debug.Trace

type Constraint = [ConstraintItem]
data ConstraintItem =
  EqType PerlType PerlType
  | EqArgs PerlArgs PerlArgs
  deriving Show
type Context = [(PerlVars, PerlType)]
data TypeContext = TypeContext {
  names :: TypeNames
  , context :: Context
  }
type TypeNames = [String]

freshName :: State TypeContext String
freshName = do
  name <- gets (head . names)
  modify (\s -> s {names = (tail . names) s})
  return name

typeNames :: TypeNames
typeNames = map (('a' :) . show) [(1 :: Integer)..]

buildConstraint :: PerlAST -> (PerlType, Constraint)
buildConstraint t = (ty, cns)
  where (ty, cns) = evalState (buildConstraint' t)
                         (TypeContext {names = typeNames, context = []})

buildConstraint' :: PerlAST -> State TypeContext (PerlType, Constraint)
buildConstraint' (PerlSubDeclare v t) = do
  (ty', cns) <- buildConstraint' t
  modify (\tc -> tc {context = (v, ty'):context tc})
  return (TypeUnknown, cns)
buildConstraint' (PerlDeclare v t) = do
  (ty', cns) <- buildConstraint' t
  modify (\tc -> tc {context = (v, ty'):context tc})
  return (ty', cns)
buildConstraint' (PerlStr _) = return (TypeBuiltin TypeStr, [])
buildConstraint' (PerlInt _) = return (TypeBuiltin TypeInt, [])
buildConstraint' (PerlVar v) = do
  ctx <- gets context
  case lookup v ctx of
    Just ty' -> return (ty', [])
    -- Should report as errors
    _ -> return (TypeUnknown, [EqType TypeUnknown TypeUnknown])
buildConstraint' (PerlImplicitItem n) = do
  (ty, c) <- buildConstraint' (PerlVar VarSubImplicit)
  name <- freshName
  let newType = TypeVar (TypeNamed name)
  nameRow1 <- freshName
  let newRow1 = ArgNamed nameRow1 M.empty
  nameRow2 <- freshName
  let newRow2 = ArgNamed nameRow2 (M.fromList [(n, newType)])
  return (newType, (EqArgs newRow1 newRow2):
                   (EqType ty (TypeArg newRow1)):c)
buildConstraint' (PerlOp op t1 t2) = do
  (ty1, c1) <- buildConstraint' t1
  (ty2, c2) <- buildConstraint' t2
  return (returnType op,
          (EqType ty1 $ leftType op):(EqType ty2 $ rightType op):c1 ++ c2)
buildConstraint' (PerlAbstract t) = do
  name <- freshName
  let newType = TypeVar (TypeNamed name)
  ctx <- gets context
  modify (\tc -> tc {context = (VarSubImplicit, newType):ctx})
  (ty, c) <- (buildConstraint' t)
  modify (\tc -> tc {context = ctx}) -- restore ctx
  return (TypeArrow newType ty, c)
buildConstraint' (PerlApp t1 ts) = do
    name <- freshName
    (ty1, c1) <- buildConstraint' t1
    (ty2, c2) <- buildArgConstraint ts
    let newType = TypeVar . TypeNamed $ name
    let c = EqType ty1 (TypeArrow ty2 newType)
    return (newType, c : c2 ++ c1)
    where
      buildArgConstraint ts' = do
        answers <- mapM buildConstraint' ts'
        let argCols = M.fromList $ zip [0..] (map fst answers)
        let constraints = concat $ map snd answers
        return (TypeArg (ArgEmpty argCols), constraints)
buildConstraint' (PerlSeq t1 t2) = do
    (_, c1) <- buildConstraint' t1
    (ty, c2) <- buildConstraint' t2
    return (ty, c2 ++ c1)

type TypeError = String

-- TypeArg PerlArgs
unify :: Constraint -> Either TypeError Substitute
unify [] = return []
unify ((EqType type1 type2):cs) = case (type1, type2) of
  (TypeUnknown, _) -> Left "not defined"
  (t1, t2@TypeUnknown) -> unify ((EqType t2 t1):cs)
  (t1, t2) | t1 == t2 -> unify cs
  (t1, t2@(TypeVar _)) | isntVar t1 -> unify ((EqType t2 t1):cs)
    where isntVar (TypeVar _) = False
          isntVar _ = True
  (TypeVar v, b@(TypeBuiltin _)) -> do
    ss <- unify (substC [SubstType v b] cs)
    return ((SubstType v b) : ss)
  (TypeVar v, t)
    | not $ t `elemTypeType` v ->
      do let subst = SubstType v t
         ss <- unify (substC (subst:[]) cs)
         return (subst:ss)
  (TypeArrow t1 t1', TypeArrow t2 t2') ->
    unify ((EqType t1 t2):(EqType t1' t2'):cs)
  (TypeArg arg1, TypeArg arg2) -> unify ((EqArgs arg1 arg2):cs)
  (t1, t2) -> Left ("Couldn't find answer:" ++ show t1 ++ "==" ++ show t2)
unify (c@(EqArgs a1 a2):cs) = isntRecursive c >> case (a1, a2) of
  (ArgEmpty m, ArgEmpty m')
    | M.null lackM && M.null lackM' -> unify (newConstraints ++ cs)
    | otherwise -> Left ("Don't match rows of const arguments:"
                         ++ show m ++ "," ++ show m')
    where
      (newConstraints, lackM, lackM') = typesToConstr m m'
  (ArgNamed s m, ArgEmpty m')
    | M.null lackM' ->
       let substs = [SubstArgs s (ArgEmpty lackM)]
           constr = newConstraints ++ cs
           constr' = substC substs constr
       in do substs' <- unify constr'
             return (substs ++ substs')
    | otherwise -> Left ("Oops, " ++ show m' ++ " has other keys:" ++ show m)
    where
      (newConstraints, lackM, lackM') = typesToConstr m m'
  (ArgEmpty _, ArgNamed _ _) -> unify ((EqArgs a2 a1):cs)
  (ArgNamed s m, ArgNamed s' m')
    -- Should I check if m or m' is empty?
    | s == s' -> unify (EqArgs (ArgEmpty m) (ArgEmpty m'):cs)
    | otherwise ->
      let newName = s ++ "'" -- TODO: It's not new name!
          substs = [
            SubstArgs s (ArgNamed newName lackM)
            , SubstArgs s' (ArgNamed newName lackM')
            ]
          constr = newConstraints ++ cs
          constr' = substC substs constr
      in do substs' <- unify constr'
            return (substs ++ substs')
    where
      (newConstraints, lackM, lackM') = typesToConstr m m'

isntRecursive :: ConstraintItem -> Either String ()
isntRecursive (EqType _ _) = error "[BUG]Not Implemented"
isntRecursive (EqArgs a b) = isntRecursive' a b >> isntRecursive' b a
  where
    isntRecursive' (ArgEmpty _) _ = return ()
    isntRecursive' (ArgNamed n _) (ArgEmpty m) =
      if elemMapArgs m n then Left ("recursive row variable " ++ n)
                         else return ()
    isntRecursive' (ArgNamed n _) (ArgNamed _ m) =
      if elemMapArgs m n then Left ("recursive row variable " ++ n)
                         else return ()

typesToConstr :: Ord k => M.Map k PerlType -> M.Map k PerlType ->
                 (Constraint, M.Map k PerlType, M.Map k PerlType)
typesToConstr m m' = (constraints, deleteKeys sames m', deleteKeys sames m)
  where
    constraints = map (\k -> EqType (unsafeLookup k m) (unsafeLookup k m'))
                      sames
    sames = sameKeys m m'
    unsafeLookup k m'' = let Just v = M.lookup k m'' in v

elemTypeType :: PerlType -> PerlTypeVars -> Bool
elemTypeType (TypeVar t) v = v == t
elemTypeType (TypeArg args) v = elemArgType args v
elemTypeType (TypeArrow t1' t2') v =
  t1' `elemTypeType` v || t2' `elemTypeType` v
elemTypeType _ _ = False

elemArgType :: PerlArgs -> PerlTypeVars -> Bool
elemArgType (ArgEmpty m) v = elemMapType m v
elemArgType (ArgNamed _ m) v = elemMapType m v
elemMapType :: M.Map k PerlType -> PerlTypeVars -> Bool
elemMapType m v = M.foldr (\ty b -> b || elemTypeType ty v)
                          False m

elemArgArgs :: PerlArgs -> String -> Bool
elemArgArgs (ArgEmpty m) name = elemMapArgs m name
elemArgArgs (ArgNamed s m) name
  | s == name = True
  | otherwise = elemMapArgs m name
elemTypeArgs :: PerlType -> String -> Bool
elemTypeArgs (TypeArg args) str = elemArgArgs args str
elemTypeArgs _ _ = False
elemMapArgs :: M.Map k PerlType -> String -> Bool
elemMapArgs m name = M.foldr (\t b -> b || elemTypeArgs t name)
                             False m

substC :: Substitute -> Constraint -> Constraint
substC subst constr = map substConst' constr
  where substConst' (EqType a b) = EqType (substType' a) (substType' b)
        substConst' (EqArgs a b) = EqArgs (substArgs' a) (substArgs' b)
        substType' = substType subst
        substArgs' = substArgs subst

infer :: PerlAST -> Either TypeError PerlType
infer t = do
  let (t', c) = buildConstraint t
  s <- unify c
  return (substType s t')
