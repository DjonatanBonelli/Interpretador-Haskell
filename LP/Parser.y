{
module Parser where 

import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }

%token 
    num         { TokenNum $$ }
    true        { TokenTrue }
    false       { TokenFalse }
    '+'         { TokenAdd }
    '-'         { TokenSub }
    '*'         { TokenMult }
    and         { TokenAnd }
    or          { TokenOr }
    not         { TokenNot }
    '<'         { TokenLess }
    "=="        { TokenEqual }  
    '='         { TokenAtrib }     
    var         { TokenVar $$ }
    if          { TokenIf }
    then        { TokenThen }
    else        { TokenElse }
    let         { TokenLet }
    in          { TokenIn }
    '\\'        { TokenLam }
    ':'         { TokenColon }
    "->"        { TokenArrow }
    Number      { TokenTNum }
    Boolean     { TokenTBool }
    '('         { TokenLParen }
    ')'         { TokenRParen }
    ','         { TokenLCons }
    '['         { TokenLBracket }
    ']'         { TokenRBracket }
    empty       { TokenLEmpty }
    head        { TokenLHead }
    tail        { TokenLTail }
    isEmpty     { TokenLIsEmpty }

%nonassoc if then else
%left '+' '-'
%left '*'

%%

Exp     : num                               { Num $1 }
        | true                              { BTrue }
        | false                             { BFalse }
        | Exp '+' Exp                       { Add $1 $3 } 
        | Exp '-' Exp                       { Sub $1 $3 }
        | Exp '*' Exp                       { Mult $1 $3 }
        | Exp and Exp                       { And $1 $3 }
        | Exp or Exp                        { Or $1 $3 }
        | not Exp                           { Not $2 }
        | '(' Exp ')'                       { Paren $2 }
        | '\\' var ':' Type "->" Exp        { Lam $2 $4 $6 }
        | Exp Exp                           { App $1 $2 }
        | var                               { Var $1 }
        | Exp "==" Exp                      { Equal $1 $3 }
        | Exp '<' Exp                       { Less $1 $3 }
        | if Exp then Exp else Exp          { If $2 $4 $6 }
        | let var '=' Exp in Exp            { Let $2 $4 $6 }
        | '[' ']'                             { LEmpty }
        | '[' ListElems ']'                     { $2 }
        | head Exp                          { LHead $2 }
        | tail Exp                          { LTail $2 }
        | isEmpty Exp                       { LIsEmpty $2 }


ListElems : Exp ',' ListElems             { LCons $1 $3 }
          | Exp                           { LCons $1 LEmpty }

Type    : Boolean                       { TBool }
        | Number                        { TNum }
        | '[' Type ']'                  { TList $2 }
        | '(' Type "->" Type ')'        { TFun $2 $4 }

{

parseError :: [Token] -> a
parseError _ = error "Erro Sintático"
}
