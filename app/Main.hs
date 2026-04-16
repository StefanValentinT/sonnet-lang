module Main (main) where

import System.Environment (getArgs)
import Lib

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> do
      content <- readFile fileName
      compile content

    _ -> putStrLn "Error: Please provide exactly one filename."
