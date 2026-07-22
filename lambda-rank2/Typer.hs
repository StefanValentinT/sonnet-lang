module Typer
  ( Constraint(..)
  , Env
  , Subst
  , initialEnv
  , inferTopLevel
  , addEnv
  ) where

import qualified Data.Set as S
import qualified Data.Map as M
import Syntax

data Constraint
  = CSub Type Type
  | CEqual Type Type
  deriving (Eq)

instance Show Constraint where
  show (CSub t1 t2)   = show t1 ++ " ≤ " ++ show t2
  show (CEqual t1 t2) = show t1 ++ " ≡ " ++ show t2

type Subst = M.Map String Type
type Env   = M.Map String Type

-- s.P
data UnificationProblem = UnificationProblem
  { freshVars   :: [String]
  , constraints :: [Constraint]
  } deriving (Eq, Show)

initialEnv :: Env
initialEnv = M.fromList
  [ ("+",     TForall ["t"] ((TInt --> TInt --> TInt) /\ (TInt --> TRef (TVar "t") --> TRef (TVar "t")) /\ (TRef (TVar "t") --> TInt --> TRef (TVar "t"))))
  , ("-",     TArr TInt (TArr TInt TInt))
  , ("suc",   TArr TInt TInt)
  , ("pred",  TArr TInt TInt)
  , ("not",   TArr TBool TBool)
  , ("if",    TForall ["t"] (TArr TBool (TArr (TVar "t") (TArr (TVar "t") (TVar "t")) )))
  , ("true",  TBool)
  , ("false", TBool)
  , ("0",     TInt)
  , ("add",   (TInt --> TInt --> TInt) /\(TBool --> TBool --> TBool) )
  ]

ftvType :: Type -> S.Set String
ftvType (TVar x)        = S.singleton x
ftvType (TArr t1 t2)    = S.union (ftvType t1) (ftvType t2)
ftvType (TAnd ts)       = S.unions (map ftvType ts)
ftvType (TForall xs t)  = S.difference (ftvType t) (S.fromList xs)
ftvType (TRef t)        = ftvType t
ftvType _               = S.empty

ftvEnv :: Env -> S.Set String
ftvEnv env = S.unions (map ftvType (M.elems env))

freshIn :: S.Set String -> String
freshIn used = case [ name | i <- [(0::Int)..], let name = "t" ++ show i, not (name `S.member` used) ] of
  (firstFresh : _) -> firstFresh
  []               -> error "Unreachable"

flattenAnd :: Type -> [Type]
flattenAnd (TAnd ts) = concatMap flattenAnd ts
flattenAnd t         = [t]

makeAnd :: [Type] -> Type
makeAnd []  = error "Empty intersection"
makeAnd [t] = t
makeAnd ts  = TAnd (concatMap flattenAnd ts)

makeForall :: [String] -> Type -> Type
makeForall [] t = t
makeForall xs (TForall ys t) = TForall (xs ++ ys) t
makeForall xs t = TForall xs t

addEnv :: Env -> Env -> Env
addEnv a1 a2 = M.unionWith (\t1 t2 -> makeAnd [t1, t2]) a1 a2

applySubst :: Subst -> Type -> Type
applySubst s (TVar x)       = M.findWithDefault (TVar x) x s
applySubst s (TArr t1 t2)   = TArr (applySubst s t1) (applySubst s t2)
applySubst s (TAnd ts)      = makeAnd (map (applySubst s) ts)
applySubst s (TForall xs t) =
  let s' = foldr M.delete s xs
  in makeForall xs (applySubst s' t)
applySubst s (TRef t)       = TRef (applySubst s t)
applySubst _ t              = t

applyEnv :: Subst -> Env -> Env
applyEnv s env = M.map (applySubst s) env

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = M.map (applySubst s1) s2 `M.union` s1

gen :: Env -> Type -> Type
gen env t =
  let envFtv  = ftvEnv env
      tFtv    = ftvType t
      genVars = S.toList (S.difference tFtv envFtv)
  in makeForall genVars t

instantiateForall :: S.Set String -> Type -> Type
instantiateForall used (TForall xs t) =
  let (subst, _) = foldl (\(m, u) x ->
        let fresh = freshIn u
        in (M.insert x (TVar fresh) m, S.insert fresh u)
        ) (M.empty, used) xs
  in applySubst subst t
instantiateForall _ t = t

freshenPair :: S.Set String -> (Env, Type) -> (Env, Type)
freshenPair used (env, t) =
  let varsToRename = S.toList (S.union (ftvEnv env) (ftvType t))
      (subst, _) = foldl (\(m, u) v ->
        let fresh = freshIn u
        in (M.insert v (TVar fresh) m, S.insert fresh u)
        ) (M.empty, used) varsToRename
  in (applyEnv subst env, applySubst subst t)

subtypeSatisfaction :: S.Set String -> Type -> Type -> UnificationProblem

subtypeSatisfaction _used s (TAnd ts) =
  UnificationProblem
    { freshVars   = []
    , constraints = [CSub s t | t <- ts]
    }

subtypeSatisfaction used (TAnd ts) tau =
  let candidates = [ subtypeSatisfaction used t tau | t <- ts ]
  in case [ p | p <- candidates, isRight (mgs (constraints p)) ] of
        (firstValid : _) -> firstValid
        []               -> UnificationProblem [] [CEqual (TAnd ts) tau]
  where
    isRight (Right _) = True
    isRight _         = False

subtypeSatisfaction used (TForall xs s) tau =
  let (subst, freshVs, _) = foldl (\(m, fVs, u) x ->
        let fresh = freshIn u
        in (M.insert x (TVar fresh) m, fresh : fVs, S.insert fresh u)
        ) (M.empty, [], used) xs
      s' = applySubst subst s
  in UnificationProblem
      { freshVars   = freshVs
      , constraints = [CSub s' tau]
      }

subtypeSatisfaction used (TArr s1 s2) (TVar tv) =
  let t1Name = freshIn used
      t2Name = freshIn (S.insert t1Name used)
      t1     = TVar t1Name
      t2     = TVar t2Name
  in UnificationProblem
      { freshVars   = [t1Name, t2Name]
      , constraints = [CSub t1 s1, CSub s2 t2, CEqual (TVar tv) (TArr t1 t2)]
      }

subtypeSatisfaction used (TVar tv) (TArr s1 s2) =
  let t1Name = freshIn used
      t2Name = freshIn (S.insert t1Name used)
      t1     = TVar t1Name
      t2     = TVar t2Name
  in UnificationProblem
      { freshVars   = [t1Name, t2Name]
      , constraints = [CEqual (TVar tv) (TArr t1 t2), CSub t1 s1, CSub t2 s2]
      }

subtypeSatisfaction _used (TArr s1 s2) (TArr t1 t2) =
  UnificationProblem 
    { freshVars   = []
    , constraints = [CSub t1 s1, CSub s2 t2]
    }

subtypeSatisfaction _used s t =
  UnificationProblem 
    { freshVars   = []
    , constraints = [CEqual s t]
    }

reduceToEqualities :: S.Set String -> [Constraint] -> [Constraint]
reduceToEqualities _ [] = []
reduceToEqualities used (CEqual a b : cs) = 
    CEqual a b : reduceToEqualities used cs
reduceToEqualities used (CSub a b : cs) =
    let (UnificationProblem freshVs newCs) = subtypeSatisfaction used a b
        used' = used `S.union` S.fromList freshVs
    in reduceToEqualities used' (newCs ++ cs)

unify :: [Constraint] -> Either String Subst
unify [] = Right M.empty
unify (CEqual t1 t2 : cs)
  | t1 == t2 = unify cs
unify (CEqual (TArr s1 s2) (TArr t1 t2) : cs) =
  unify (CEqual s1 t1 : CEqual s2 t2 : cs)
unify (CEqual (TRef s) (TRef t) : cs) =
  unify (CEqual s t : cs)
unify (CEqual (TVar tv) t : cs)
  | tv `S.member` ftvType t = Left $ "Occurs check failed for " ++ tv
  | otherwise =
      let s = M.singleton tv t
          substC (CEqual a b) = CEqual (applySubst s a) (applySubst s b)
          substC c = c
      in do
        u <- unify (map substC cs)
        Right (composeSubst u s)
unify (CEqual t (TVar tv) : cs) =
  unify (CEqual (TVar tv) t : cs)
unify (CEqual t1 t2 : _) =
  Left $ "Unification error: mismatch between " ++ show t1 ++ " and " ++ show t2
unify (CSub _ _ : _) =
  Left "Unexpected CSub remaining during unification"

mgs :: [Constraint] -> Either String Subst
mgs cs = 
  let extractVars (CSub a b) = ftvType a `S.union` ftvType b
      extractVars (CEqual a b) = ftvType a `S.union` ftvType b
      initialUsed = S.unions (map extractVars cs)
  in unify (reduceToEqualities initialUsed cs)

pp :: Env -> Term -> Either String (Env, Type)
pp env (Var x) = case M.lookup x env of
  Just ty -> 
    let instTy = instantiateForall (ftvType ty) ty
    in Right (M.empty, instTy)
  Nothing -> 
    let t = TVar ("t_" ++ x)
    in Right (M.singleton x t, t)

pp env (Abs x n) = case pp env n of
  Left err -> Left err
  Right (a, s) ->
    case M.lookup x a of
      Just tyX ->
        let aPrime = M.delete x a
        in Right (aPrime, gen aPrime (TArr tyX s))
      Nothing ->
        let used  = S.union (ftvEnv a) (ftvType s)
            tName = freshIn used
            t     = TVar tName
        in Right (a, gen a (TArr t s))

pp env (App m1 m2) = case (pp env m1, pp env m2) of
  (Left err, _) -> Left err
  (_, Left err) -> Left err
  (Right (a1, s1), Right pair2) ->
    let
      usedVars1   = S.union (ftvEnv a1) (ftvType s1)
      (a2, s2)    = freshenPair usedVars1 pair2
      usedVarsAll = S.unions [usedVars1, ftvEnv a2, ftvType s2]
      alphaName   = freshIn usedVarsAll
      alpha       = TVar alphaName
      cs          = [ CSub s1 (TArr s2 alpha) ]
    in case mgs cs of
      Left err -> Left $ "App satisfaction failed: " ++ err
      Right u  ->
        let mergedEnv = applyEnv u (addEnv a1 a2)
            resType   = applySubst u alpha
        in Right (mergedEnv, resType)

pp env (Rec x n) = case pp env n of
  Left err -> Left err
  Right (a, s) ->
    case M.lookup x a of
      Nothing -> Right (a, gen a s)
      Just tyX ->
        let aPrime   = M.delete x a
            sigma    = gen aPrime s
            cs       = [ CSub sigma tyX ]
        in case mgs cs of
          Left err -> Left $ "Rec satisfaction failed: " ++ err
          Right u  ->
            let mergedEnv = applyEnv u aPrime
                resType   = gen mergedEnv (applySubst u s)
            in Right (mergedEnv, resType)

inferTopLevel :: Env -> Term -> Either String (Env, Type)
inferTopLevel env ast = do
  (reqEnv, ty) <- pp env ast
  pure (addEnv env reqEnv, gen env ty)