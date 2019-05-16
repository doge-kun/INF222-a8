module Parser (program, parse) where

import Syntax
import System.IO
import Control.Monad

import Text.Parsec

import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = words "true false var if while fun function ref return try catch reset shift spawn detach join \\ -> ."
           , Token.reservedOpNames = words "+ - * / % == != < > <= >= && || ! ="
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis
braces     = Token.braces     lexer -- parses surrounding braces
brackets   = Token.brackets   lexer -- parses surrounding brackets
integer    = Token.integer    lexer -- parses an integer
natural    = Token.natural    lexer -- parses a natural number
semi       = Token.semi       lexer -- parses a semicolon
symbol     = Token.symbol     lexer -- parses one of the ops
whiteSpace = Token.whiteSpace lexer -- parses whitespace
commaSep   = Token.commaSep   lexer -- parses a comma separated list
stringlit  = Token.stringLiteral lexer
top = parse program

program = do
  whiteSpace
  ss <- many statement
  eof
  return $ SBlock $ foldr SSeq SSkip ss

statement =
  empty <|>
  ifStmt <|>
  whileStmt <|>
  block <|>
  returnStmt <|>
  tryStmt <|>
  throwStmt <|>
  varDeclStmt <|>
  assignStmt <|>
  exprStmt

empty = semi >> return SSkip
ifStmt = do
  reserved "if"
  e <- parens expr
  s1 <- statement
  reserved "else"
  s2 <- statement
  return $ SIf e s1 s2
whileStmt = do
  reserved "while"
  e <- parens expr
  s <- statement
  return $ SWhile e s
block = do
  ss <- braces (many statement)
  return $ SBlock $ foldr SSeq SSkip ss
assignStmt = do
  i <- try ( identifier >>= \j -> reservedOp "=" >> return j )
  e <- expr
  semi
  return $ SAssign i e
varDeclStmt = do
  reserved "var"
  i <- identifier
  reservedOp "="
  e <- expr
  semi
  return $ SVarDecl i e
returnStmt = do
  reserved "return"
  e <- expr
  semi
  return $ SRet e
tryStmt = do
  reserved "try"
  s1 <- statement
  reserved "catch"
  e <- parens identifier
  s2 <- statement
  return $ STry s1 e s2
throwStmt = do
  reserved "throw"
  s <- stringLiteral
  semi
  return $ SThrow s
exprStmt = do
  e <- expr
  semi
  return $ SExpr e

expr = conjunction `chainl1` binOp "||"
conjunction = relation `chainl1` binOp "&&"
relation = do
  l <- summation
  (anyBinOp (words "== != < <= > >=") >>= \o -> summation >>= \r -> return $ o l r) <|> return l
summation = term `chainl1` anyBinOp (words "+ -")
term = factor `chainl1` anyBinOp (words "* / %")
factor = literal <|> fun <|> lambda <|> atomicOrCall <|> ref <|> list
literal = intLiteral <|> boolLiteral "false" False <|> boolLiteral "true" True <|> stringLiteral

intLiteral = EVal . VInt . fromInteger <$> natural
boolLiteral s v = reserved s >> (return $ EVal (VBool v))
stringLiteral = EVal . VString <$> stringlit

ref = ERef <$> (reserved "ref" >> factor)
deref = EDeref <$> (reservedOp "*" >> atomic)

binOp s = reservedOp s >> (return $ (\a b -> ECall (EVar ("__b" ++ s)) [a, b] []))

anyBinOp ops = foldl1 (<|>) (map binOp ops)

list = EList <$> brackets (commaSep expr)

fun = do
  reserved "fun" <|> reserved "function"
  pars <- parens (commaSep identifier)
  body <- block
  return $ EFun pars body

lambda = do
  reserved "\\"
  pars <- (symbol "_" >> pure []) <|> parens (commaSep identifier)
  reserved "->" <|> reserved "."
  body <- braces expr <|> expr
  return $ ELambda pars body

variable = EVar <$> identifier

resetExpr = EReset <$> (reserved "reset" >> parens (fun <|> lambda))
shiftExpr = EShift <$> (reserved "shift" >> parens (fun <|> lambda))

atomic = resetExpr <|> shiftExpr
  <|> variable <|> parens expr
  <|> deref
  -- <|> spawnExpr <|> detachExpr <|> joinExpr

atomicOrCall = do
  a <- atomic
  argss <- many (parens (commaSep expr))
  return $ foldl (\a arg -> ECall a arg []) a argss
