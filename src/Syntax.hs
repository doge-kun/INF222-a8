module Syntax where

import Data.IORef
import Data.List

data Ast
  = SSkip
  | Hole
  | HoleWithEnv Env

 -- Statements
  | SIf Expr Stmt Stmt
  | SWhile Expr Stmt
  | SBlock Stmt
  | SSeq Stmt Stmt
  | SAssign String Expr
  | SVarDecl String Expr
  | SExpr Expr
  | SRet Expr
  | STry Stmt String Stmt
  | SThrow Expr

 -- Expressions
  | EVal Value
  | EVar String
  | EFun [String] Stmt
  | ECall Expr [Expr] [Value]
  | ERef Expr
  | EDeref Expr
  | EList [Expr]
  | ELambda [String] Expr
  | EReset Expr
  | EShift Expr
  deriving Show

type Stmt = Ast
type Expr = Ast
type Ctx = Ast

data Value
  = VInt Int
  | VBool Bool
  | VString String
  | VRef (IORef Value)
  | VVoid
  | VClosure [String] Stmt Env
  | VPrimFun ([Value] -> Value)
  | VPrimFunIO ([Value] -> IO Value)
  | VList [Value]
  | VCont Env [Ctx]

isValue, notValue :: Ast -> Bool
isValue (EVal _) = True
isValue _ = False
notValue = not . isValue

expr2val :: Expr -> Value
expr2val (EVal v) = v

isReset :: Expr -> Bool
isReset (EReset _) = True
isReset _ = False

type Env = [(String, Value)]

instance Show Value where
  show (VInt i) = show i
  show (VBool b) = show b
  show (VString s) = s
  show (VRef r) = "ref"
  show (VVoid) = "void"
  show (VClosure _ _ _) = "closure"
  show (VPrimFun _) = "prim-fun"
  show (VPrimFunIO _) = "prim-fun io"
  show (VList vs) = "[" ++ intercalate "," (map show vs) ++ "]"
  show (VCont _ _) = "continuation"
