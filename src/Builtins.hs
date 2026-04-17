module Builtins (makeBuiltins, isBuiltin) where

import qualified Data.Map.Strict as Map
import SimpleType
import Syntax (Type)

makeBuiltins :: IO (Map.Map String SimpleType)
makeBuiltins = do
    a <- freshVar
    let boolty = SPrimBool
    let binaryIntFields = Map.fromList [("0", SPrimI32), ("1", SPrimI32)]
    let arthty = SFunction (SRecord binaryIntFields) SPrimI32
    let arthboolty = SFunction (SRecord binaryIntFields) SPrimBool
    let eqTy = SFunction (SRecord (Map.fromList [("0", a), ("1", a)])) SPrimBool

    return $
        Map.fromList
            [ ("true", boolty)
            , ("false", boolty)
            , ("+", arthty)
            , ("-", arthty)
            , ("*", arthty)
            , ("/", arthty)
            , ("eq?", eqTy)
            , ("not", SFunction SPrimBool SPrimBool)
            , (">", arthboolty)
            , ("<", arthboolty)
            ]

isBuiltin :: String -> Bool
isBuiltin name = name `elem` ["+", "-", "*", "/", "eq?", ">", "<", "not", "true", "false"]
