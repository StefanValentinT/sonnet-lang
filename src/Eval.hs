module Eval (eval, evalProgram) where

import Builtins
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Syntax

type EvalCtx = Map String TypedTerm

eval :: (TypedTerm, EvalCtx) -> (TypedTerm, EvalCtx)
eval (t, ctx) = case t of
    TFun _ _ _ -> (t, ctx)
    TLitInt _ -> (t, ctx)
    TRecord fs ty ->
        (TRecord (Map.map (\tm -> fst (eval (tm, ctx))) fs) ty, ctx)
    TIdent n ty ->
        case Map.lookup n ctx of
            Just (TIdent n2 _) | n == n2 -> (t, ctx)
            Just body -> eval (body, ctx)
            Nothing | isBuiltin n -> (t, ctx)
            Nothing -> error $ "Variable " <> n <> " not found!"
    TApp f arg _ ->
        let (vFun, _) = eval (f, ctx)
            (vArg, _) = eval (arg, ctx)
         in case vFun of
                TFun param body _ ->
                    eval (body, Map.insert param vArg ctx)
                TIdent name _
                    | isBuiltin name ->
                        (applyBuiltin name vArg, ctx)
                _ -> error $ "Runtime Error: Cannot apply " ++ show vFun
    TLetIn name rhs body _ ->
        let (vRhs, _) = eval (rhs, ctx)
            newCtx = Map.insert name vRhs ctx
         in eval (body, newCtx)
    TSel container fieldName _ ->
        case eval (container, ctx) of
            (TRecord fs _, _) ->
                case Map.lookup fieldName fs of
                    Just valTerm -> (valTerm, ctx)
                    Nothing -> error $ "Runtime Error: Field " ++ fieldName ++ " not found."
            _ -> error "Runtime Error: Selection target is not a record."
    TIf cond th el _ ->
        case eval (cond, ctx) of
            (TIdent "true" _, _) -> eval (th, ctx)
            (TIdent "false" _, _) -> eval (el, ctx)
            _ -> error "Runtime Error: 'if' condition did not evaluate to a boolean."

evalProgram :: TypedProgram -> TypedTerm
evalProgram pgrm =
    case Map.lookup "main" initialCtx of
        Just mainBody -> fst (eval (mainBody, initialCtx))
        Nothing -> error "Runtime Error: 'main' is not defined."
  where
    initialCtx = Map.fromList [(name, body) | (name, body) <- Set.toList pgrm]

applyBuiltin :: String -> TypedTerm -> TypedTerm
applyBuiltin name (TRecord fs _) =
    let val0 = Map.lookup "0" fs
        val1 = Map.lookup "1" fs
     in case (val0, val1) of
            (Just (TLitInt a), Just (TLitInt b)) ->
                case name of
                    "+" -> TLitInt (a + b)
                    "-" -> TLitInt (a - b)
                    "*" -> TLitInt (a * b)
                    "/" -> if b == 0 then error "Division by zero" else TLitInt (a `div` b)
                    ">" -> if a > b then TIdent "true" (TypeVar "Bool") else TIdent "false" (TypeVar "Bool")
                    "<" -> if a < b then TIdent "true" (TypeVar "Bool") else TIdent "false" (TypeVar "Bool")
                    "eq?" -> if a == b then TIdent "true" (TypeVar "Bool") else TIdent "false" (TypeVar "Bool")
                    _ -> error $ "Builtin " ++ name ++ " not implemented for these arguments"
            _ -> error $ "Builtin " ++ name ++ " expected integer record [0=i, 1=j]"
applyBuiltin "not" (TIdent "true" _) = TIdent "false" (TypeVar "Bool")
applyBuiltin "not" (TIdent "false" _) = TIdent "true" (TypeVar "Bool")
applyBuiltin name _ = error $ "Invalid arguments for builtin " ++ name
