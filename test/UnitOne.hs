{-# LANGUAGE QuasiQuotes #-}
module UnitOne where

import Test.Tasty.HUnit
import Helpers
import Syntax
import Eval (evalProgram)
import Text.RawString.QQ (r)

unit_arithmeticType :: Assertion
unit_arithmeticType = 
    assertProgramTypes "(def main (+ 1 2))" "main" I32

unit_mutualRecursionTypes :: Assertion
unit_mutualRecursionTypes = 
    let code = [r|
        (def is_even 
            (fun n -> 
                (if (eq? n 0) 
                 then true 
                 else (is_odd (- n 1))))
        )

        (def is_odd 
            (fun n -> 
                (if (eq? n 0) 
                 then false 
                 else (is_even (- n 1))))
        )
    |]
    in assertProgramTypes code "is_even" (FunType I32 Bool)
