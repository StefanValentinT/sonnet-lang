module Main where
import System.IO (hFlush, stdout, stdin, stderr, hSetEncoding, utf8)
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import qualified Data.Map as M

import Syntax
import Parser
import Typer

main :: IO ()
main = do
  hSetEncoding stdin  utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  repl initialEnv

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

repl :: Env -> IO ()
repl env = do
  putStr ">>> "
  hFlush stdout
  input <- getLine
  let trimmed = trim input

  if trimmed `elem` [":q", ":quit"]
    then putStrLn "Goodbye!"
    else if null trimmed
      then repl env
      else do
        env' <- handleAssign env trimmed
        repl env'

handleAssign :: Env -> String -> IO Env
handleAssign env input
  | "=" `isInfixOf` input && not (":" `isPrefixOf` input) = do
      let (varPart, rest) = break (== '=') input
          varName         = trim varPart
          termStr         = trim (drop 1 rest)

      if null varName
        then putStrLn "  [Error]: Missing variable name before '='" >> return env
        else case parseTerm termStr of
          Left err -> putStrLn ("  [Parse Error]: " ++ err) >> return env
          Right ast -> case pp env ast of
            Left err -> putStrLn ("  [Type Error]: " ++ err) >> return env
            Right (reqEnv, ty) -> do
              let combinedEnv = addEnv env reqEnv
              let newEnv      = M.insert varName ty combinedEnv
              putStrLn $ "  " ++ varName ++ " : " ++ show ty
              return newEnv

  | otherwise = do
      putStrLn "  [Error]: Input must be in the format 'name = term'"
      return env

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (needle `isPrefixOf`) (tails haystack)
  where
    tails [] = [[]]
    tails xs@(_:xs') = xs : tails xs'