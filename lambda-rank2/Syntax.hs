module Syntax where

import Data.List (intercalate)

data Term
  = Var String
  | App Term Term
  | Abs String Term
  | Rec String Term
  | Let String Term Term
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