module Helpers (assertProgramTypes, assertProgramResult) where

import Test.Tasty.HUnit
import Parser (parseProgram)
import Typer (infer)
import Eval (evalProgram)
import Syntax
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

assertProgramTypes :: String -> String -> Type -> Assertion
assertProgramTypes input defName expectedType = do
    case parseProgram input of
        Left err -> assertFailure $ "Parse Error: " ++ show err
        Right program -> do
            typedProgram <- infer program
            let typeMap = Map.fromList (Set.toList typedProgram)
            
            case Map.lookup defName typeMap of
                Just term -> 
                    getType term @?= expectedType
                Nothing -> 
                    assertFailure $ "Definition " ++ defName ++ " not found in typed program"

getType :: TypedTerm -> Type
getType term = case term of
    TIdent _ ty          -> ty
    TLitInt _            -> I32
    TApp _ _ ty          -> ty
    TFun _ _ ty          -> ty
    TLetIn _ _ _ ty      -> ty
    TRecord _ ty         -> ty
    TSel _ _ ty          -> ty
    TIs _ _ _ _ ty       -> ty
    TIf _ _ _ ty         -> ty

assertProgramResult :: String -> TypedTerm -> Assertion
assertProgramResult input expected = do
    case parseProgram input of
        Left err -> assertFailure $ "Parse Error: " ++ show err
        Right program -> do
            typedProgram <- infer program
            let result = evalProgram typedProgram
            result @?= expected