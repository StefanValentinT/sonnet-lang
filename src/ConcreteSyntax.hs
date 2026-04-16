module ConcreteSyntax (CType (..), CTypedTerm (..), CTypedProgram (..)) where

type CTypedProgram = [CTypedTerm]

data CTypedTerm
    = TIdent String CType
    | TLitInt Int
    | TApp CTypedTerm CTypedTerm CType
    | TFun String CTypedTerm CType
    | TLetIn String CTypedTerm CTypedTerm CType
    | TRecord [(String, CTypedTerm)]
    | TIs CTypedTerm CType CTypedTerm CTypedTerm
    | TIf CTypedTerm CTypedTerm CTypedTerm CType
    deriving (Show, Eq)

data CType
    = CTop
    | CBot
    | CBool
    | CI32
    | CUnion CType CType
    | CInter CType CType
    | CNeg CType
    | CRecType String CType
    | CRec [(String, CType)]
    | CFunType CType CType
    deriving (Show, Eq)
