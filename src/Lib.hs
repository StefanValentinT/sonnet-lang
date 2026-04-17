module Lib (compile) where

import Control.Monad (forM_)
import Eval
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
            typedProgram <- infer program
            pPrint typedProgram
            pPrint $ evalProgram typedProgram

-- let loProg = specialise typedProgram

-- pPrint loProg
