{
module Syntax.Parser where

import Syntax.Lexer
import Syntax.Token
}

%name parse
%tokentype { Token }
%monad { IO } { (>>=) } { return }
%error { parseError }

%token

'while'                                 { TokKeyword (KwWhile, $$) }
'for'                                   { TokKeyword (KwFor, $$) }
'to'                                    { TokKeyword (KwTo, _) }
'break'                                 { TokKeyword (KwBreak, $$) }
'let'                                   { TokKeyword (KwLet, $$) }
'in'                                    { TokKeyword (KwIn, $$) }
'end'                                   { TokKeyword (KwEnd, _) }
'function'                              { TokKeyword (KwFunction, $$) }
'var'                                   { TokKeyword (KwVar, $$) }
'type'                                  { TokKeyword (KwType, $$) }
'array'                                 { TokKeyword (KwArray, $$) }
'if'                                    { TokKeyword (KwIf, $$) }
'then'                                  { TokKeyword (KwThen, _) }
'else'                                  { TokKeyword (KwElse, _) }
'do'                                    { TokKeyword (KwDo, _) }
'of'                                    { TokKeyword (KwOf, _) }
'nil'                                   { TokKeyword (KwNil, _) }

','                                     { TokSymbol (SymComma, $$) }
':'                                     { TokSymbol (SymColon, _) }
';'                                     { TokSymbol (SymSemi, $$) }
'('                                     { TokSymbol (SymLParen, $$) }
')'                                     { TokSymbol (SymRParen, _) }
'['                                     { TokSymbol (SymLBrack, $$) }
']'                                     { TokSymbol (SymRBrack, _) }
'{'                                     { TokSymbol (SymLBrace, _) }
'}'                                     { TokSymbol (SymRBrace, _) }
'.'                                     { TokSymbol (SymDot, $$) }
'+'                                     { TokSymbol (SymPlus, $$) }
'-'                                     { TokSymbol (SymMinus, $$) }
'*'                                     { TokSymbol (SymTimes, $$) }
'/'                                     { TokSymbol (SymDivide, $$) }
'='                                     { TokSymbol (SymEQ, $$) }
'<>'                                    { TokSymbol (SymNEQ, $$) }
'<'                                     { TokSymbol (SymLT, $$) }
'<='                                    { TokSymbol (SymLE, $$) }
'>'                                     { TokSymbol (SymGT, $$) }
'>='                                    { TokSymbol (SymGE, $$) }
'&'                                     { TokSymbol (SymAnd, $$) }
'|'                                     { TokSymbol (SymOr, $$) }
':='                                    { TokSymbol (SymAssign, $$) }

id                                      { TokId $$ }
int                                     { TokInt $$ }
string                                  { TokString $$ }


%left '+' '-'
%left '*' '/'
%left UMINUS
%left '|' '&'
%nonassoc '<' '<=' '>' '>='

%%

program         :: {} 
                : exp                                                   {}

decs            :: {}
                : dec decs                                              {}
                | {- empty -}                                           {}

dec             :: {}
                : tydecs                                                {}
                | vardec                                                {}
                | fundecs                                               {}

tydecs          :: {}
                : tydec tydecs                                          {}
                | tydec                                                 {}

tydec           :: {}
                : 'type' tyid '=' ty                                    {}

ty              :: {}
                : tyid                                                  {}
                | '{' tyfields '}'                                      {}
                | 'array' 'of' tyid                                     {}

tyfields        :: {}
                : varid ':' tyid ',' tyfields                           {}
                | varid ':' tyid                                        {}
                | {- empty -}                                           {}

vardec          :: {}
                : 'var' id ':=' exp                                     {}
                | 'var' id ':' tyid ':=' exp                            {}

fundecs         :: {}
                : fundec fundecs                                        {}                                    
                | fundec                                                {}

fundec          :: {}
                : 'function' varid '(' tyfields ')' '=' exp             {}
                | 'function' varid '(' tyfields ')' ':' tyid '=' exp    {}

lvalue          :: {}
                : varid                                                 {}
                | lvalue '.' varid                                      {}
                | lvalue '[' exp ']'                                    {}

exp             :: {}
                : lvalue                                                {}
                | 'nil'                                                 {}
                | '(' seqexp ')'                                        {}
                | int                                                   {}
                | string                                                {}
                | '-' exp %prec UMINUS                                  {}
                | varid '(' args ')'                                    {}
                | exp '+' exp                                           {}
                | exp '-' exp                                           {}
                | exp '*' exp                                           {}
                | exp '/' exp                                           {}
                | exp '=' exp                                           {}
                | exp '<>' exp                                          {}
                | exp '<' exp                                           {}
                | exp '<=' exp                                          {}
                | exp '>' exp                                           {}
                | exp '>=' exp                                          {}
                | exp '&' exp                                           {}
                | exp '|' exp                                           {}
                | varid '{' rcd '}'                                     {}
                | varid '[' exp ']' 'of' exp                            {}
                | lvalue ':=' exp                                       {}
                | 'if' exp 'then' exp 'else' exp                        {}
                | 'if' exp 'then' exp                                   {}
                | 'while' exp 'do' exp                                  {}
                | 'for' id ':=' exp 'to' exp 'do' exp                   {}
                | 'break'                                               {}
                | 'let' decs 'in' seqexp 'end'                          {}

tyid            :: {}
                : id                                                    {}

varid           :: {}
                : id                                                    {}

seqexp          :: {}
                : exps                                                  {}

exps            :: {}
                : exp ';' exps                                          {}
                | exp                                                   {}
                | {- empty -}                                           {}

args            :: {}
                : exp ',' args                                          {}
                | exp                                                   {}
                | {- empty -}                                           {}

rcd             :: {}
                : varid '=' exp ',' rcd                                 {}
                | varid '=' exp                                         {}
                | {- empty -}                                           {}

{
parseError :: [Token] -> IO a
parseError [] = fail "parse error at EOF"
parseError (t : _) = fail $ "parse error at " ++ show t
}