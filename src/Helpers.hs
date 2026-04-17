module Helpers (getType, canonicalize) where

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
