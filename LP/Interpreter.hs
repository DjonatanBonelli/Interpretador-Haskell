module Interpreter where

import Lexer

step :: Expr -> Expr

-- Operadores Aritméticos
step (Add (Num n1) (Num n2)) = Num (n1 + n2) -- S-Add
step (Add (Num n1) e2) = let e2' = step e2  -- S-Add2
                          in Add ( Num n1) e2'
step (Add e1 e2) = Add (step e1) e2  -- S-Add1 

step (Sub (Num n1) (Num n2)) = Num (n1 - n2)
step (Sub (Num n1) e2) = let e2' = step e2
                            in Sub (Num n1) e2'
step (Sub e1 e2) = Sub (step e1) e2

step (Mult (Num n1) (Num n2)) = Num (n1 * n2)
step (Mult (Num n1) e2) = let e2' = step e2
                            in Mult (Num n1) e2'
step (Mult e1 e2) = Mult (step e1) e2

-- Operadores Lógicos
step (And (BTrue) (e2)) = (e2)
step (And (BFalse) (e2)) = (BFalse)
step (And e1 e2) = And (step e1) e2

step (Or (BTrue) (e2)) = (BTrue)
step (Or (BFalse) (e2)) = (e2)
step (Or e1 e2) = Or (step e1) e2

step (Not (BTrue)) = (BFalse)
step (Not (BFalse)) = (BTrue)
step (Not e1) = Not (step e1)

-- Operadores Relacionais
step (Equal (Num n1) (Num n2)) = if n1 == n2 then BTrue else BFalse

step (Equal BTrue BTrue)   = BTrue
step (Equal BFalse BFalse) = BTrue
step (Equal BTrue BFalse)  = BFalse
step (Equal BFalse BTrue)  = BFalse

step (Equal v1 e2) | isValue v1 = Equal v1 (step e2)
step (Equal e1 e2)              = Equal (step e1) e2

step (Less (Num n1) (Num n2)) = if n1 < n2 then BTrue else BFalse
step (Less (Num n1) e2) = let e2' = step e2
                          in Less (Num n1) e2'
step (Less e1 e2) = Less (step e1) e2

-- Operadores de Fluxo
step (If BTrue e1 e2) = e1
step (If BFalse e1 e2) = e2
step (If e e1 e2) = If (step e) e1 e2

-- Variáveis (let)
step (Let v e1 e2)
  | isValue e1 = subst v e1 e2 
  | otherwise        =   Let v (step e1) e2

-- Cálculo Lambda
step (App e1@(Lam x t b) e2) | isValue e2 = subst x e2 b 
                           | otherwise = App e1 (step e2)
step (App e1 e2) = App (step e1) e2

step (Paren e) = e 

-- Listas
step (LCons h t)
  | not (isValue h)     = LCons (step h) t
  | not (isValue t)     = LCons h (step t)

step (LHead (LCons h _))
  | isValue h           = h
step (LHead e) = LHead (step e)

step (LTail (LCons _ t))
  | isValue t           = t
step (LTail e) = LTail (step e)

step (LIsEmpty LEmpty)        = BTrue
step (LIsEmpty (LCons _ _))   = BFalse

-- Reconhece como valor
isValue :: Expr -> Bool
isValue (Num _)     = True
isValue BTrue       = True
isValue BFalse      = True
isValue (Lam _ _ _) = True
isValue LEmpty      = True
isValue (LCons h t) = isValue h && isValue t
isValue _           = False

-- Avalia a função inteira
eval :: Expr -> Expr
eval e
  | isValue e = e
  | otherwise = eval (step e)

-- Substituição na Avaliação
subst :: String -> Expr -> Expr -> Expr
subst v e BTrue = BTrue
subst v e BFalse = BFalse
subst v e (Num x) = Num x
subst v e (Add e1 e2) = Add (subst v e e1) (subst v e e2)
subst v e (Sub e1 e2) = Sub (subst v e e1) (subst v e e2)
subst v e (Mult e1 e2) = Mult (subst v e e1) (subst v e e2)
subst v e (And e1 e2) = And (subst v e e1) (subst v e e2)
subst v e (Or e1 e2) = Or (subst v e e1) (subst v e e2)
subst v e (Not e1) = Not (subst v e e1)
subst v e (If e1 e2 e3) = If (subst v e e1) (subst v e e2) (subst v e e3)
subst v e (Less e1 e2) = Less (subst v e e1) (subst v e e2)
subst v e (Equal e1 e2) = Equal (subst v e e1) (subst v e e2)

subst v e (Var x) = if v == x then 
                       e
                    else
                       Var x
subst v e (Lam x t b) = Lam x t (subst v e b)
subst v e (App e1 e2) = App (subst v e e1) (subst v e e2)
subst v e (Paren e1) = Paren (subst v e e1)
subst v e (Let s e1 e2) = Let s e1 (subst v e e2)
subst v e LEmpty = LEmpty
subst v e (LCons h t) = LCons (subst v e h) (subst v e t)
subst v e (LHead l)   = LHead (subst v e l)
subst v e (LTail l)   = LTail (subst v e l)
subst v e (LIsEmpty l)  = LIsEmpty (subst v e l)