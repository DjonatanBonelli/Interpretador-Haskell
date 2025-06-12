module Lexer where

import Data.Char

data Expr = BTrue
          | BFalse
          | Num Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr
          | And Expr Expr
          | Or Expr Expr
          | Not Expr
          | Equal Expr Expr
          | Less Expr Expr
          | If Expr Expr Expr
          | Var String
          | Lam String Ty Expr
          | Let String Expr Expr
          | App Expr Expr
          | Paren Expr 
          | LEmpty          
          | LCons Expr Expr         
          | LHead Expr              
          | LTail Expr                 
          | LIsEmpty Expr          
          deriving Show

data Ty = TBool
        | TNum
        | TList Ty
        | TFun Ty Ty
        deriving (Show, Eq)

data Token = TokenTrue
           | TokenFalse
           | TokenNum Int
           | TokenAdd 
           | TokenSub
           | TokenMult
           | TokenAnd
           | TokenOr
           | TokenNot
           | TokenEqual
           | TokenLess
           | TokenAtrib
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenVar String 
           | TokenLam 
           | TokenArrow 
           | TokenColon
           | TokenTNum 
           | TokenTBool
           | TokenLParen 
           | TokenRParen  
           | TokenLet
           | TokenIn 
           | TokenLEmpty
           | TokenLCons
           | TokenLHead     -- head
           | TokenLTail     -- tail
           | TokenLIsEmpty  -- empty
           | TokenLBracket  -- [
           | TokenRBracket  -- ]
           deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (',':cs) = TokenLCons : lexer cs
lexer ('\\':cs) = TokenLam : lexer cs 
lexer ('-':'>':cs) = TokenArrow : lexer cs 
lexer ('+':cs) = TokenAdd : lexer cs
lexer ('-':cs) = TokenSub : lexer cs
lexer ('*':cs) = TokenMult : lexer cs
lexer ('=':'=':cs) = TokenEqual : lexer cs
lexer ('=':cs) = TokenAtrib : lexer cs
lexer (':':cs) = TokenColon : lexer cs 
lexer ('<':cs) = TokenLess : lexer cs
lexer ('(':cs) = TokenLParen : lexer cs 
lexer (')':cs) = TokenRParen : lexer cs 
lexer ('[':cs) = TokenLBracket : lexer cs
lexer (']':cs) = TokenRBracket : lexer cs
lexer (c:cs) | isSpace c = lexer cs 
             | isDigit c = lexNum (c:cs) 
             | isAlpha c = lexKW (c:cs)


lexNum :: String -> [Token]
lexNum cs = case span isDigit cs of
              (num, rest) -> TokenNum (read num) : lexer rest

lexKW :: String -> [Token]
lexKW cs = case span isAlpha cs of
             ("true", rest) -> TokenTrue : lexer rest
             ("false", rest) -> TokenFalse : lexer rest
             ("if", rest) -> TokenIf : lexer rest
             ("then", rest) -> TokenThen : lexer rest
             ("else", rest) -> TokenElse : lexer rest
             ("and", rest) -> TokenAnd : lexer rest
             ("or", rest) -> TokenOr : lexer rest
             ("not", rest) -> TokenNot : lexer rest
             ("Number", rest) -> TokenTNum : lexer rest 
             ("Boolean", rest) -> TokenTBool : lexer rest 
             ("let", rest) -> TokenLet : lexer rest
             ("in", rest) -> TokenIn : lexer rest
             ("empty", rest)     -> TokenLEmpty : lexer rest
             ("head", rest)      -> TokenLHead : lexer rest
             ("tail", rest)      -> TokenLTail : lexer rest
             ("isEmpty", rest)   -> TokenLIsEmpty : lexer rest
             (var, rest) -> TokenVar var : lexer rest 
 
             --_ -> error "Erro léxico: palavra chave inváida" 