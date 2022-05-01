{
module Syntax.Parser where

import Syntax.Token
import Syntax.Lexer
}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexer } { TokEof }
%error { parseError }

%token

'while'                 { TokKeyword (KwWhile, $$) }
'for'                   { TokKeyword (KwFor, $$) }
'to'                    { TokKeyword (KwTo, _) }
'break'                 { TokKeyword (KwBreak, $$) }
'let'                   { TokKeyword (KwLet, $$) }
'in'                    { TokKeyword (KwIn, $$) }
'end'                   { TokKeyword (KwEnd, _) }
'function'              { TokKeyword (KwFunction, $$) }
'var'                   { TokKeyword (KwVar, $$) }
'type'                  { TokKeyword (KwType, $$) }
'array'                 { TokKeyword (KwArray, $$) }
'if'                    { TokKeyword (KwIf, $$) }
'then'                  { TokKeyword (KwThen, _) }
'else'                  { TokKeyword (KwElse, _) }
'do'                    { TokKeyword (KwDo, _) }
'of'                    { TokKeyword (KwOf, _) }
'nil'                   { TokKeyword (KwNil, _) }

','                     { TokSymbol (SymComma, $$) }
':'                     { TokSymbol (SymColon, _) }
';'                     { TokSymbol (SymSemi, $$) }
'('                     { TokSymbol (SymLParen, $$) }
')'                     { TokSymbol (SymRParen, _) }
'['                     { TokSymbol (SymLBrack, $$) }
']'                     { TokSymbol (SymRBrack, _) }
'{'                     { TokSymbol (SymLBrace, _) }
'}'                     { TokSymbol (SymRBrace, _) }
'.'                     { TokSymbol (SymDot, $$) }
'+'                     { TokSymbol (SymPlus, $$) }
'-'                     { TokSymbol (SymMinus, $$) }
'*'                     { TokSymbol (SymTimes, $$) }
'/'                     { TokSymbol (SymDevide, $$) }
'='                     { TokSymbol (SymEq, $$) }
'<>'                    { TokSymbol (SymNeq, $$) }
'<'                     { TokSymbol (SymLt, $$) }
'<='                    { TokSymbol (SymLe, $$) }
'>'                     { TokSymbol (SymGt, $$) }
'>='                    { TokSymbol (SymGe, $$) }
'&'                     { TokSymbol (SymAnd, $$) }
'|'                     { TokSymbol (SymOr, $$) }
':='                    { TokSymbol (SymAssign, $$) }

id                      { TokId $$ }
int                     { TokInt $$ }
string                  { TokString $$ }


%left '+' '-'
%left '*' '/'
%left UMINUS
%left '|' '&'
%nonassoc '<' '<=' '>' '>='

%%

program : exp   {}

decs :: {}
    : dec decs          {}
    | {- empty -}       {}

dec :: {}
    : tydec     {}
    | vardec    {}
    | fundec    {}

tydec :: {}
    : 'type' id '=' ty      {}

ty  :: {}
    : id                    {}
    | '{' tyfields '}'      {}
    | 'array' 'of' id       {}

tyfields :: {}
    : id ':' id tyfields_       {}
    | {- empty -}               {}

tyfields_ :: {}
    : ',' id ':' id tyfields_       {}
    | {- empty -}                   {}

vardec :: {}
    : 'var' id ':=' exp               {}
    | 'var' id ':' id ':=' exp        {}

fundec :: {}
    : 'function' id  '(' tyfields ')' '=' exp             {}
    | 'function' id '(' tyfields ')' ':' id '=' exp       {}

lvalue :: {}
    : id                            {}
    | id '.' id                     {}
    | lvalue '.' id                 {}
    | id '[' exp ']'                {}
    | lvalue '[' exp ']'            {}

exp :: {}
    : lvalue                                {}
    | 'nil'                                 {}
    | '(' seqexp ')'                        {}
    | int                                   {}
    | string                                {}
    | '-' exp %prec UMINUS                  {}
    | id '(' args ')'                       {}
    | exp '+' exp                           {}
    | exp '-' exp                           {}
    | exp '*' exp                           {}
    | exp '/' exp                           {}
    | exp '=' exp                           {}
    | exp '<>' exp                          {}
    | exp '<' exp                           {}
    | exp '<=' exp                          {}
    | exp '>' exp                           {}
    | exp '>=' exp                          {}
    | exp '&' exp                           {}
    | exp '|' exp                           {} 
    | id '{' rcd '}'                        {}
    | id '[' exp ']' 'of' exp               {}
    | lvalue ':=' exp                       {}
    | 'if' exp 'then' exp 'else' exp        {}
    | 'if' exp 'then' exp                   {}
    | 'while' exp 'do' exp                  {}
    | 'for' id ':=' exp 'to' exp 'do' exp   {}
    | 'break'                               {}
    | 'let' decs 'in' seqexp 'end'          {}

seqexp :: {}
    : exp seqexp_               {}
    | {- empty -}               {}
    
seqexp_ :: {}
    : ';' exp seqexp_           {}
    | {- empty -}               {}

args :: {}
    : exp args_                 {}
    | {- empty -}               {}

args_ :: {}
    : ',' exp args_             {}
    | {- empty -}               {}

rcd :: {}
    : id '=' exp rcd_           {}
    | {- empty -}               {}

rcd_ :: {}
    : ',' id '=' exp rcd_       {}
    | {- empty -}               {}


{
parseError :: Token -> Alex a
parseError t = alexError $ "Parse error: " ++ show t
}