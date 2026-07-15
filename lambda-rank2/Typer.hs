module Typer 
  ( Constraint(..)
  , Env
  , Subst
  , initialEnv
  , infer
  ) where

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
type Env = M.Map String Type

initialEnv :: Env
initialEnv = M.fromList
  [ ("+",      TArr TInt (TArr TInt TInt))
  , ("-",      TArr TInt (TArr TInt TInt))
  , ("not",    TArr TBool TBool)
  , ("true",   TBool)
  , ("false",  TBool)
  , ("1",      TInt)
  ]

infer :: Term -> Either String (Env, Type, [Constraint])
infer term = Right (M.empty, TInt, [])
