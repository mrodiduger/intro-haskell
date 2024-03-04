-- abstract syntax trees

module Ast where

-- expression
data Exp t
  = Variable t
  | Const Integer
  | Plus (Exp t) (Exp t)
  | Less (Exp t) (Exp t)
  | And (Exp t) (Exp t)
  | Not (Exp t)
  | IfThenElse (Exp t) (Exp t) (Exp t)

instance (Show t) => Show (Exp t) where
  show :: Exp t -> String
  show (Variable x) = show x
  show (Const n) = show n
  show (Plus e1 e2) = "(" ++ show e1 ++ ") + (" ++ show e2 ++ ")"
  show (Less e1 e2) = "(" ++ show e1 ++ ") < (" ++ show e2 ++ ")"
  show (And e1 e2) = "(" ++ show e1 ++ ") && (" ++ show e2 ++ ")"
  show (Not e) = "!(" ++ show e ++ ")"
  show (IfThenElse e1 e2 e3) = "if (" ++ show e1 ++ ") then (" ++ show e2 ++ ") else (" ++ show e3 ++ ")"

-- environment which assigns a value to a variable
type Env t = t -> Integer

-- evaluate an expression in an environment
eval :: Env t -> Exp t -> Integer
eval env (Variable x) = env x
eval env (Const n) = n
eval env (Plus e1 e2) = eval env e1 + eval env e2
eval env (Less e1 e2) = if eval env e1 < eval env e2 then 1 else 0
eval env (And e1 e2) = if eval env e1 /= 0 && eval env e2 /= 0 then 1 else 0
eval env (Not e) = if eval env e == 0 then 1 else 0
eval env (IfThenElse e1 e2 e3) = if eval env e1 /= 0 then eval env e2 else eval env e3

-- an example expression
example :: Exp Char
example = IfThenElse (Less (Variable 'x') (Const 0)) (Plus (Variable 'x') (Const 1)) (Const 0)

-- an example environment
exampleEnv :: Env Char
exampleEnv 'x' = 42
