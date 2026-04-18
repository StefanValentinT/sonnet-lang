module Lib (compile) where

import Control.Monad (forM_)
import Eval
import Helpers
import Parser (parseProgram)
import Specialise
import Syntax
import Text.Megaparsec (errorBundlePretty)
import Text.Pretty.Simple (pPrint)
import Typer (infer)

compile :: String -> IO ()
compile input = do
    case parseProgram input of
        Left err -> do
            putStrLn (errorBundlePretty err)
        Right program -> do
            pPrint program
            typedProgram <- infer program
            pPrint typedProgram
            printTypedProgram typedProgram

-- let loProg = specialise typedProgram

-- pPrint loProg
