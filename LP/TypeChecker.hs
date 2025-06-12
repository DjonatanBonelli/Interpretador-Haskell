module TypeChecker where

import Lexer

type Ctx = [(String, Ty)]

typeof :: Ctx -> Expr -> Maybe Ty

typeof ctx (Num _) = Just TNum

typeof ctx BFalse = Just TBool
typeof ctx BTrue = Just TBool

-- Operadores Aritméticos
typeof ctx (Add e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                      (Just TNum, Just TNum) -> Just TNum
                      _                      -> Nothing
typeof ctx (Mult e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                      (Just TNum, Just TNum) -> Just TNum
                      _                      -> Nothing      
typeof ctx (Sub e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                      (Just TNum, Just TNum) -> Just TNum
                      _                      -> Nothing                 
-- Operadores Lógicos
typeof ctx (And e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                      (Just TBool, Just TBool) -> Just TBool
                      _                        -> Nothing
typeof ctx (Or e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                      (Just TBool, Just TBool) -> Just TBool
                      _                        -> Nothing
typeof ctx (Not e1) = case (typeof ctx e1) of
                      (Just TBool) -> Just TBool
                      _             -> Nothing
-- Operadores Relacionais
typeof ctx (Equal e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                          (Just TNum, Just TNum)   -> Just TBool
                          (Just TBool, Just TBool) -> Just TBool
                          _                        -> Nothing
typeof ctx (Less e1 e2) = case (typeof ctx e1, typeof ctx e2) of
                        (Just TNum, Just TNum) -> Just TBool
                        _                      -> Nothing
-- Operadores de Fluxo
typeof ctx (If e1 e2 e3) = 
    case (typeof ctx e1) of
      Just TBool -> case (typeof ctx e2, typeof ctx e3) of   
                      (Just t1, Just t2) | t1 == t2 -> Just t1
                                         | otherwise -> Nothing
                      _ -> Nothing
      _ -> Nothing

-- Calculo Lambda
typeof ctx (Var v) = lookup v ctx
typeof ctx (Lam x t1 b) = let ctx' = (x, t1) : ctx in 
                          case typeof ctx' b of
                            Just t2 -> Just (TFun t1 t2)
                            _       -> Nothing

typeof ctx (App e1 e2) = 
  case (typeof ctx e1) of
    Just (TFun t11 t12) -> case typeof ctx e2 of
                          Just t2 -> if t11 == t2 then
                                        Just t12
                                      else
                                        Nothing
                          _ -> Nothing

typeof ctx (Paren e) = typeof ctx e 

-- Variáveis: Let
typeof ctx (Let v e1 e2) = case typeof ctx e1 of
                          Just t1 ->  typeof ((v, t1) : ctx) e2
                          _ -> Nothing

-- Listas
typeof _ LEmpty = Just (TList TNum)

typeof ctx (LCons x xs) = case (typeof ctx x, typeof ctx xs) of
  (Just t1, Just (TList t2)) | t1 == t2 -> Just (TList t1)
  _ -> Nothing

typeof ctx (LHead e) = case typeof ctx e of
                         Just (TList t) -> Just t
                         _              -> Nothing

typeof ctx (LTail e) = case typeof ctx e of
                         Just (TList t) -> Just (TList t)
                         _              -> Nothing

typeof ctx (LIsEmpty e) = case typeof ctx e of
                            Just (TList _) -> Just TBool
                            _              -> Nothing

typecheck :: Expr -> Expr
typecheck expr = case typeof [] expr of
                   Just _ -> expr
                   _ -> error "Erro de tipos"
