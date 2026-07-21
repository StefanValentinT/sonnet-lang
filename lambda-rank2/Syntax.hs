module Syntax where

import Data.List (intercalate)

data Term
  = Var String
  | App Term Term
  | Abs String Term
  | Rec String Term
  deriving (Eq, Show)

data Type
  = TVar String
  | TArr Type Type
  | TAnd [Type]
  | TForall [String] Type
  | TInt
  | TBool
  | TRef Type
  deriving (Eq)

instance Show Type where
  show (TVar x)       = x
  show (TArr t1 t2)   = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
  show (TAnd [])      = "Empty"
  show (TAnd [t])     = show t
  show (TAnd ts)      = "(" ++ intercalate " ∩ " (map show ts) ++ ")"
  show (TForall xs t) = "∀" ++ unwords xs ++ ". " ++ show t
  show TInt           = "Int"
  show TBool          = "Bool"
  show (TRef t)       = "*" ++ show t


infixr 3 /\
(/\) :: Type -> Type -> Type
TAnd ts /\ TAnd us = TAnd (ts ++ us)
TAnd ts /\ t       = TAnd (ts ++ [t])
t       /\ TAnd us = TAnd (t : us)
t1      /\ t2      = TAnd [t1, t2]

infixl 9 $$
($$) :: Term -> Term -> Term
($$) = App

infixr 0 -->
(-->) :: Type -> Type -> Type
(-->) = TArr