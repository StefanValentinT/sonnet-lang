{-# LANGUAGE QuasiQuotes #-}
module RecordTest where

import Test.Tasty.HUnit
import TestHelpers
import Syntax
import Text.RawString.QQ (r)
import Parser (parseType)

unit_selExecution :: Assertion
unit_selExecution = 
    let code = [r|
        (def main (sel [100 200] 0))
    |]
    in assertProgramResult code (TLitInt 100)

unsafeParseType :: String -> Type
unsafeParseType s = case parseType s of
    Left err -> error $ "Test Error: Invalid expected type string: " ++ show err
    Right ty -> ty

unit_pickObject :: Assertion
unit_pickObject =
    let code = [r|
        (def a [42 true])
        (def b [17 (fun x -> x)])
        (def pick (fun x -> (if x then a else b)))
    |]
        expected = unsafeParseType "fun Bool -> [I32 (union Bool (fun Top -> Bot))]"
    in assertProgramTypes code "pick" expected