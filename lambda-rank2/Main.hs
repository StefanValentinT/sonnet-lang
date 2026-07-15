module Main where

import System.IO (hFlush, stdout)
import Data.Char (isSpace)
import Data.List (intercalate)
import qualified Data.Map as M

import Syntax
import Parser (parseTerm)
import Typer (Constraint, Env, initialEnv, infer)

main :: IO ()
main = repl

repl :: IO ()
repl = do
  putStr ">>> "
  hFlush stdout
  input <- getLine
  if input `elem` [":q", ":quit"]
    then putStrLn "Goodbye!"
    else if all isSpace input
      then repl
      else do
        case parseTerm input of
          Left err -> putStrLn $ "  [Parse Error]: " ++ err
          Right ast -> do
            case infer ast of
              Left err -> putStrLn $ "  [Type Error]:  " ++ err
              Right (env, ty, residualCs) -> do
                let reqEnv = M.difference env initialEnv
                putStrLn $ "  [Inferred Type]:     " ++ show ty
                putStrLn $ "  [Constraints Held]:  " ++ showConstraints residualCs
                putStrLn $ "  [Context Required]:  " ++ showEnv reqEnv
        repl

showConstraints :: [Constraint] -> String
showConstraints cs = "{" ++ intercalate ", " (map show cs) ++ "}"

showEnv :: Env -> String
showEnv env = "{" ++ intercalate ", " [ x ++ " : " ++ show t | (x, t) <- M.toList env ] ++ "}"