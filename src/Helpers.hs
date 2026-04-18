module Helpers (getType, canonicalize, prettyPrintType, printTypedProgram) where

import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Syntax
import Typer (infer)

getType :: TypedTerm -> Type
getType term = case term of
    TIdent _ ty -> ty
    TLitInt _ -> I32
    TApp _ _ ty -> ty
    TFun _ _ ty -> ty
    TLetIn _ _ _ ty -> ty
    TRecord _ ty -> ty
    TSel _ _ ty -> ty
    TIf _ _ _ ty -> ty
    TIs _ _ _ _ ty -> ty

canonicalize :: Type -> Type
canonicalize (Union a b) =
    let a' = canonicalize a
        b' = canonicalize b
     in if show a' < show b' then Union a' b' else Union b' a'
canonicalize (FunType a b) = FunType (canonicalize a) (canonicalize b)
canonicalize (RecordType m) = RecordType (Map.map canonicalize m)
canonicalize t = t

prettyPrintType :: Type -> String
prettyPrintType Top = "Top"
prettyPrintType Bot = "Bot"
prettyPrintType Bool = "Bool"
prettyPrintType I32 = "Int"
prettyPrintType (Union t1 t2) =
    "(" ++ prettyPrintType t1 ++ " ∨ " ++ prettyPrintType t2 ++ ")"
prettyPrintType (Inter t1 t2) =
    "(" ++ prettyPrintType t1 ++ " ∧ " ++ prettyPrintType t2 ++ ")"
prettyPrintType (Neg t) = "¬" ++ prettyPrintType t
prettyPrintType (FunType t1 t2) =
    "(" ++ prettyPrintType t1 ++ " → " ++ prettyPrintType t2 ++ ")"
prettyPrintType (RecordType m) =
    "{" ++ intercalate ", " [k ++ ": " ++ prettyPrintType v | (k, v) <- Map.toList m] ++ "}"
prettyPrintType (RecType name t) = "μ" ++ name ++ "." ++ prettyPrintType t
prettyPrintType (TypeVar s) = s

printTypedProgram :: TypedProgram -> IO ()
printTypedProgram prog = mapM_ printEntry (Set.toList prog)
  where
    printEntry (name, term) =
        putStrLn $ name ++ " : " ++ prettyPrintType (getType term)
