module TestHelpers (assertProgramTypes, assertProgramResult) where

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
                Just term -> do
                    let actualType = getType term
                    canonicalize actualType @?= canonicalize expectedType
                Nothing -> 
                    assertFailure $ "Definition " ++ defName ++ " not found."

assertProgramResult :: String -> TypedTerm -> Assertion
assertProgramResult input expected = do
    case parseProgram input of
        Left err -> assertFailure $ "Parse Error: " ++ show err
        Right program -> do
            typedProgram <- infer program
            let result = evalProgram typedProgram
            result @?= expected