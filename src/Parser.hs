module Parser
    ( parseProgram
    , parseTerm
    , parseType
    ) where

import Control.Monad (void)
import Data.Char (isDigit)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void)
import Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String
type ParseErr = ParseErrorBundle String Void

sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

keywords :: [String]
keywords =
    [ "let"
    , "in"
    , "fun"
    , "if"
    , "then"
    , "else"
    , "is"
    , "rec"
    , "true"
    , "false"
    , "Top"
    , "Bot"
    , "Bool"
    , "I32"
    ]

typeEntryP :: Parser (Maybe String, Type)
typeEntryP =
    try ((,) . Just <$> (identifier <* symbol "=") <*> typeP)
        <|> ((,) Nothing <$> typeP)

entryP :: Parser (Maybe String, Term)
entryP =
    try ((,) . Just <$> (identifier <* symbol "=") <*> atomP)
        <|> ((,) Nothing <$> atomP)

assignLabels :: [(Maybe String, a)] -> [(String, a)]
assignLabels = snd . foldl step (0 :: Int, [])
  where
    step (n, acc) (Just lbl, t) = (n, acc ++ [(lbl, t)])
    step (n, acc) (Nothing, t) = (n + 1, acc ++ [(show n, t)])

identifier :: Parser String
identifier = lexeme . try $ do
    let forbidden = "()[]{}∧∨¬\\/~∈= \t\n\r"
    first <- satisfy (\c -> not (isDigit c) && c `notElem` forbidden)
    rest <- many (satisfy (`notElem` forbidden))
    let name = first : rest

    if name `elem` keywords
        then fail $ "reserved keyword: " ++ name
        else return name

keyword :: String -> Parser ()
keyword kw = lexeme . try $ do
    void $ string kw
    notFollowedBy (alphaNumChar <|> oneOf "_'?!")

memberSymbol :: Parser ()
memberSymbol = void (symbol "∈") <|> keyword "in"

litInt :: Parser Term
litInt = LitInt <$> lexeme L.decimal

litBool :: Parser Term
litBool =
    (keyword "true" *> pure (Ident "true"))
        <|> (keyword "false" *> pure (Ident "false"))

parseType :: String -> Either ParseErr Type
parseType src = parse (parseType' <* eof) "<type input>" src

parseType' :: Parser Type
parseType' = sc *> typeP

typeP :: Parser Type
typeP =
    parens compoundTypeP
        <|> brackets recordTypeP
        <|> try compoundTypeP
        <|> atomicTypeP

atomicTypeP :: Parser Type
atomicTypeP =
    (keyword "Top" *> pure Top)
        <|> (keyword "Bot" *> pure Bot)
        <|> (keyword "Bool" *> pure Bool)
        <|> (keyword "I32" *> pure I32)
        <|> (TypeVar <$> identifier)

compoundTypeP :: Parser Type
compoundTypeP =
    (void (symbol "∨") <|> void (keyword "union"))
        *> (Union <$> typeP <*> typeP)
            <|> (void (symbol "∧") <|> void (keyword "inter"))
        *> (Inter <$> typeP <*> typeP)
            <|> (void (symbol "¬") <|> void (keyword "neg"))
        *> (Neg <$> typeP)
            <|> (keyword "rec")
        *> (RecType <$> identifier <* symbol "->" <*> typeP)
            <|> funTypeP

funTypeP :: Parser Type
funTypeP = keyword "fun" *> (FunType <$> typeP <* symbol "->" <*> typeP)

recordTypeP :: Parser Type
recordTypeP = RecordType . Map.fromList . assignLabels <$> many typeEntryP

parseTerm :: Parser Term
parseTerm = sc *> atomP

atomP :: Parser Term
atomP =
    parens sExprP
        <|> recordTermP
        <|> litInt
        <|> litBool
        <|> (Ident <$> identifier)

sExprP :: Parser Term
sExprP =
    parseBuiltinSExpr
        <|> letP
        <|> funP
        <|> ifP
        <|> isP
        <|> appP

parseBuiltinSExpr :: Parser Term
parseBuiltinSExpr = try $ do
    void $ symbol "sel"
    obj <- atomP
    field <- (identifier <|> (show <$> lexeme L.decimal))
    return (Sel obj field)

letP :: Parser Term
letP = do
    keyword "let"
    name <- identifier
    t1 <- atomP
    keyword "in"
    LetIn name t1 <$> atomP

funP :: Parser Term
funP = do
    keyword "fun"
    name <- identifier
    void $ symbol "->"
    Fun name <$> atomP

ifP :: Parser Term
ifP = do
    keyword "if"
    cond <- atomP
    keyword "then"
    t1 <- atomP
    keyword "else"
    If cond t1 <$> atomP

isP :: Parser Term
isP = do
    keyword "is"
    t <- atomP
    memberSymbol
    ty <- typeP
    keyword "then"
    t1 <- atomP
    keyword "else"
    Is t ty t1 <$> atomP

autoLabel :: [Term] -> [(String, Term)]
autoLabel ts = zip (map show [0 :: Int ..]) ts

appP :: Parser Term
appP = do
    f <- atomP
    args <- many atomP
    case args of
        [] -> return f
        [single] -> return (App f single)
        multiple -> return (App f (Record $ Map.fromList $ autoLabel multiple))

recordTermP :: Parser Term
recordTermP = brackets (Record . Map.fromList . assignLabels <$> many entryP)

defP :: Parser (String, Term)
defP = parens $ do
    keyword "def"
    name <- identifier
    body <- atomP
    return (name, body)

parseProgram :: String -> Either ParseErr Program
parseProgram src = parse (sc *> (Set.fromList <$> many defP) <* eof) "<input>" src
