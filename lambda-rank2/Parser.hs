module Parser (parseTerm) where

import Data.Char (isAlphaNum, isSpace, toLower)
import Data.List (isPrefixOf)
import Syntax (Term(..))

trim :: String -> String
trim = dropWhile isSpace

isIdChar :: Char -> Bool
isIdChar c = isAlphaNum c || c `elem` ("+-" :: String)

parseAtom :: String -> Either String (Term, String)
parseAtom s = case trim s of
  ('\\':cs) -> parseAbs cs
  ('λ':cs)  -> parseAbs cs
  cs | take 6 (map toLower cs) == "lambda" -> parseAbs (drop 6 cs)
  cs | take 3 (map toLower cs) == "rec"    -> parseRec (drop 3 cs)
  ('(':cs)  -> case parseExpr cs of
    Left err -> Left err
    Right (term, remainder) -> case trim remainder of
      (')':next) -> Right (term, next)
      _          -> Left "Expected closing parenthesis ')'"
  cs@(c:_) | isIdChar c ->
    let (name, rest) = span isIdChar cs
    in Right (Var name, rest)
  _ -> Left "Expected variable, lambda, rec, let, or parenthesized term"

parseAbs :: String -> Either String (Term, String)
parseAbs s =
  let s1 = trim s
      (var, s2) = span isIdChar s1
  in if null var
     then Left "Expected variable name after lambda"
     else case trim s2 of
       ('.':s3) -> case parseExpr s3 of
         Left err -> Left err
         Right (body, remainder) -> Right (Abs var body, remainder)
       _ -> Left "Expected '.' after lambda variable"

parseRec :: String -> Either String (Term, String)
parseRec s =
  let s1 = trim s
      (var, s2) = span isIdChar s1
  in if null var
     then Left "Expected variable name after 'rec'"
     else case trim s2 of
       ('.':s3) -> case parseExpr s3 of
         Left err -> Left err
         Right (body, remainder) -> Right (Rec var body, remainder)
       _ -> Left "Expected '.' after rec variable"

parseExpr :: String -> Either String (Term, String)
parseExpr s = case parseAtom s of
  Left err -> Left err
  Right (first, remainder) -> go first remainder
  where
    go acc r =
      let r' = trim r in
      if null r' || ")" `isPrefixOf` r'
      then Right (acc, r')
      else case parseAtom r' of
        Left _ -> Right (acc, r') 
        Right (next, rest) -> go (App acc next) rest

parseTerm :: String -> Either String Term
parseTerm s = case parseExpr s of
  Left err -> Left err
  Right (term, remainder) ->
    if null (trim remainder)
    then Right term
    else Left $ "Trailing unparsed characters: " ++ remainder