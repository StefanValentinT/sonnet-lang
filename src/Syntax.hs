module Syntax
    ( Term (..)
    , Type (..)
    , Program
    , TypedProgram
    , TypedTerm (..)
    ) where

import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Program = Set (String, Term)

data Term
    = Ident String -- x
    | LitInt Int
    | App Term Term -- (e t)
    | Fun String Term -- (fun x t)
    | LetIn String Term Term -- (let x t_1 in t_2) t_1 can be recursive and t_2 can refer to it, if x ≠ '_'
    | Record (Map.Map String Term) -- [x_1 = t_1 ... x_n = t_n]
    | Sel Term String
    | Is Term Type Term Term -- (is t_1 ∈ T t_2 t_3)
    | If Term Term Term
    deriving (Show, Eq, Ord)

data Type
    = Top -- "Top"
    | Bot -- "Bot"
    | Bool
    | I32
    | Union Type Type -- (∨ T_1 T_2)
    | Inter Type Type -- (∧ T_1 T_2)
    | Neg Type -- (¬ t)
    | RecordType (Map.Map String Type) -- [x_1 = T_1 ... x_n = T_n]
    | RecType String Type
    | FunType Type Type
    | TypeVar String
    deriving (Show, Eq, Ord)

type TypedProgram = Set (String, TypedTerm)

data TypedTerm
    = TIdent String Type -- x
    | TLitInt Int
    | TApp TypedTerm TypedTerm Type -- (e t)
    | TFun String TypedTerm Type -- (fun x t)
    | TLetIn String TypedTerm TypedTerm Type -- (let x t_1 in t_2) t_1 can be recursive and t_2 can refer to it, if x ≠ '_'
    | TRecord (Map.Map String TypedTerm) Type -- [x_1 = t_1 ... x_n = t_n]
    | TSel TypedTerm String Type
    | TIs TypedTerm Type TypedTerm TypedTerm Type -- (is t_1 ∈ T t_2 t_3)
    | TIf TypedTerm TypedTerm TypedTerm Type
    deriving (Show, Eq, Ord)
