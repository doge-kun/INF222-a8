module Eval where

import Syntax
import Primitive
import Pretty
import Debug.Trace (trace)

import Control.Monad
import Data.IORef

import Data.List
import Data.Maybe

addVar :: String -> Value -> Env -> Env
addVar s v env = (s, v):env

addVars :: [String] -> [Value] -> Env -> Env
addVars ss vs env = zip ss vs ++ env

findVar :: String -> Env -> Value
findVar s env = maybe (error errmsg) id $ lookup s env
  where errmsg = "Variable " ++ s ++ " not found in " ++ show env

-- Safe lookup
get :: [a] -> Int -> Maybe a
get xs i
  | 0 <= i && i < length xs
  = pure (xs !! i)
get _ _ = Nothing
(!?) = get

retVal :: Env -> [Ctx] -> Value -> (Ast, Env, [Ctx])
retVal env ctx x = (EVal x, env, ctx)

exec :: Ast -> IO ()
exec e = steps (e, primitives, [])

steps :: (Ast, Env, [Ctx]) -> IO ()
steps (SSkip, _, []) = return ()
steps st = step st >>= steps

step :: (Ast, Env, [Ctx]) -> IO (Ast, Env, [Ctx])
-- step (ast, e, c) | trace ((show ast) ++ "\n") False = undefined
-- step (ast, e, c) | trace ((show ast) ++ "\n--\n" ++ show c ++ "\n") False = undefined
-- step (ast, e, c) | trace ((show ast) ++ "\n--\n" ++ show e ++ "\n") False = undefined

-- Statement expression: evaluate expression and turn into SSkip
step (SExpr e, env, ctx) = return (e, env, SExpr Hole : ctx)
step (v, env, SExpr Hole : ctx) | isValue v = return (SSkip, env, ctx)

-- Blocks
step (SBlock s, env, ctx) = return (s, env, (SBlock (HoleWithEnv env)) : ctx)
step (SSkip, _, SBlock (HoleWithEnv env) : ctx) = return (SSkip, env, ctx) -- restore environment when block closes

-- Sequences
step (SSeq s1 s2, env, ctx) = return (s1, env, SSeq Hole s2 : ctx)
step (SSkip, env, SSeq Hole s2 : ctx) = return (s2, env, ctx)

-- If and while
step (SIf cond s1 s2, env, ctx) = return (cond, env, SIf Hole s1 s2 : ctx)
step (EVal (VBool True), env, SIf Hole s1 _ : ctx) = return (SBlock s1, env, ctx)
step (EVal (VBool False), env, SIf Hole _ s2 : ctx) = return (SBlock s2, env, ctx)

step (w@(SWhile cond s), env, ctx) = return (SIf cond (SSeq s w) SSkip, env, ctx)

-- Variable declaration
step (SVarDecl s e, env, ctx) = return (e, env, SVarDecl s Hole : ctx)
step (v, env, SVarDecl s Hole : ctx) | isValue v 
  = return (SSkip, addVar s (expr2val v) env, ctx)

-- Assignment
step (SAssign s e, env, ctx) = return (e, env, SAssign s Hole : ctx)
step (v, env, SAssign s Hole : ctx) | isValue v = do
  case findVar s env of
    (VRef nv) -> writeIORef nv (expr2val v) >> return (SSkip, env, ctx)
    _ -> ioError $ userError $ "Trying to assign to a non-ref \"" ++ s ++ "\""

-- Variable reference: get from environment
step (EVar s, env, ctx) = return (EVal $ findVar s env, env, ctx)

-- Box a value
step (ERef e, env, ctx) = return (e, env, ERef Hole : ctx)
step (v, env, ERef Hole : ctx) | isValue v
  = retVal env ctx . VRef <$> newIORef (expr2val v)

-- Dereference a ref
step (EDeref e, env, ctx) = return (e, env, EDeref Hole : ctx)
step (v, env, EDeref Hole : ctx)
  | isValue v
  , (VRef nv) <- expr2val v
  = retVal env ctx <$> readIORef nv

-- Function becomes a closure
step (EFun pars body, env, ctx) = return (EVal $ VClosure pars body env, env, ctx)

-- Same with lambdas, but returns by default
step (ELambda pars body, env, ctx) = return (EVal $ VClosure pars (SRet body) env, env, ctx)

-- Calls of closure, primitive function, and primitive IO functions, assuming arguments evaluated
step (ECall (EVal (VClosure pars s cloEnv)) [] vs, env, ctx) = do
  return (s, addVars pars (reverse vs) cloEnv, ECall (HoleWithEnv env) [] vs: ctx)
step (SSkip, _, ECall (HoleWithEnv env) _ _ : ctx) = return (EVal VVoid, env, ctx)
  -- function body fully evaluated, return VVoid

-- Get element from list with call
step (ECall (EVal (VList xs)) [] [VInt i], env, ctx)
  = return $ case xs !? i of
      Just x -> (EVal x, env, ctx)
      Nothing -> error $ "Index out of bounds, index " ++ show i ++ ", length " ++ show (length xs) ++ "."

-- Normal function call
step (ECall (EVal (VPrimFun f)) [] vs, env, ctx)
  = return (EVal $ f (reverse vs), env, ctx)
step (ECall (EVal (VPrimFunIO f)) [] vs, env, ctx)
  = retVal env ctx <$> f (reverse vs)

-- Continuation call
step (ECall (EVal (VCont env' ctx')) [v] [], env, ctx)
  = return (v, env ++ env', ctx' ++ (HoleWithEnv env : ctx))
step (e, _, HoleWithEnv env : ctx) = return (e, env, ctx)

step (ECall f [] _, _, _) | isValue f = error $ "a call to non-function " ++ show f

-- Reduce on function position
step (ECall f args [], env, ctx) | notValue f = return (f, env, ECall Hole args [] : ctx)
step (f, env, ECall Hole args [] : ctx) | isValue f = return (ECall f args [], env, ctx)
step (ECall f (a:args) vs, env, ctx) | isValue f = return (a, env, ECall f (Hole:args) vs : ctx)
step (v, env, ECall f (Hole:args) vs : ctx) | isValue v = return (ECall f args (expr2val v : vs), env, ctx)

-- Return statement
step (SRet e, env, ctx) | notValue e = return (e, env, SRet Hole : ctx)
step (e, env, SRet Hole : ctx) | isValue e = return (SRet e, env, ctx)
step (SRet e, _, ECall (HoleWithEnv env) _ _ : ctx) = return (e, env, ctx)
step (SRet e, env, ECall _ _ _ : ctx) = return (e, env, ctx)
step (SRet e, env, _ : ctx) = return (SRet e, env, ctx)

-- Try/catch statement
step (STry s1 evar s2, env, ctx) = return (s1, env, STry Hole evar s2 : ctx)
step (SSkip, env, STry Hole evar s2 : ctx) = return (SSkip, env, ctx)

-- Throw
step (SThrow e, env, ctx) | notValue e = return (e, env, SThrow Hole : ctx)
step (e, env, SThrow Hole : ctx) = return (SThrow e, env, ctx)
step (SThrow e, env, STry Hole evar s2 : ctx)
  = return (s2, (evar, expr2val e) : env, ctx)
step (SThrow e, env, c:ctx) = return (SThrow e, env, ctx)

-- List
step (EList vals, env, ctx) | all isValue vals = return (EVal (VList (map expr2val vals)), env, ctx)
step (EList (v:vals), env, ctx) | isValue v = do
  (EList vals', env', ctx') <- step (EList vals, env, ctx)
  return (EList (v:vals'), env', ctx')
step (EList (v:vals), env, ctx) = do
  (v', env', ctx') <- step (v, env, ctx)
  return (EList (v':vals), env', ctx')

step (EReset e, env, ctx) | notValue e = return (e, env, EReset Hole : ctx)
step (e@(EVal (VClosure _ _ _)), env, EReset Hole : ctx) = return (ECall e [] [], env, EReset Hole : ctx)
step (e, env, EReset Hole : ctx) | isValue e = return (e, env, ctx)

step (EShift e, env, ctx) | notValue e = return (e, env, EShift Hole : ctx)
step (e, env, EShift Hole : ctx)
  | (before, after) <- break isReset ctx
  , cont <- EVal $ VCont env before
  , isValue e
  = return (ECall e [cont] [], env, after)

step (e, env, ctx) = error $ "Non-exhaustive pattern: " ++ show e ++ ", ctx " ++ show ctx
