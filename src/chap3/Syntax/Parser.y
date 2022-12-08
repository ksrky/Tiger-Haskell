{
module Syntax.Parser where

import Data.IORef

import Syntax.Absyn
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

program         :: { Exp }
                : exp                                                   { $1 }

decs            :: { [Dec] }
                : dec decs                                              { $1 : $2 }
                | {- empty -}                                           { [] }

dec             :: { Dec }
                : tydecs                                                { TypeDec $1 }
                | vardec                                                { $1 }
                | fundecs                                               { FunctionDec $1 }

tydecs          :: { [(Name, Ty, Pos)] }
                : tydec tydecs                                          { $1 : $2 }
                | tydec                                                 { [$1] }

tydec           :: { (Name, Ty, Pos) }
                : 'type' tyid '=' ty                                    { (fst $2, $4, pos $1) }

ty              :: { Ty }
                : tyid                                                  { NameTy (snd $1) (fst $1) }
                | '{' tyfields '}'                                      { RecordTy $2 }
                | 'array' 'of' tyid                                     { ArrayTy (pos $1) (fst $3) }

tyfields        :: { [Field] }
                : varid ':' tyid ',' tyfields                           {% do { esc <- escape; return (Field (snd $1) (fst $1) esc (fst $3) : $5) } }
                | varid ':' tyid                                        {% do { esc <- escape; return [Field (snd $1) (fst $1) esc (fst $3)] } }
                | {- empty -}                                           { [] }

vardec          :: { Dec }
                : 'var' id ':=' exp                                     {% do { esc <- escape; return (VarDec (pos $1) (fst $2) esc Nothing $4) } }
                | 'var' id ':' tyid ':=' exp                            {% do { esc <- escape; return (VarDec (pos $1) (fst $2) esc (Just $4) $6) } }

fundecs         :: { [FunDec] }
                : fundec fundecs                                        { $1 : $2 }                                    
                | fundec                                                { [$1] }

fundec          :: { FunDec }
                : 'function' varid '(' tyfields ')' '=' exp             { FunDec (pos $1) (fst $2) $4 Nothing $7 }
                | 'function' varid '(' tyfields ')' ':' tyid '=' exp    { FunDec (pos $1) (fst $2) $4 (Just (fst $7, snd $7)) $9 }

lvalue          :: { Var }
                : varid                                                 { SimpleVar (snd $1) (fst $1) }
                | lvalue '.' varid                                      { FieldVar (pos $2) $1 (fst $3) }
                | lvalue '[' exp ']'                                    { SubscriptVar (pos $2) $1 $3 }

exp             :: { Exp }
                : lvalue                                                { VarExp $1 }
                | 'nil'                                                 { NilExp }
                | '(' seqexp ')'                                        { $2 }
                | int                                                   { IntExp (fst $1) }
                | string                                                { StringExp (pos (snd $1)) (fst $1) }
                | '-' exp %prec UMINUS                                  { OpExp (pos $1) (IntExp 0) MinusOp $2 }
                | varid '(' args ')'                                    { CallExp (snd $1) (fst $1) $3 }
                | exp '+' exp                                           { OpExp (pos $2) $1 PlusOp $3 }
                | exp '-' exp                                           { OpExp (pos $2) $1 MinusOp $3 }
                | exp '*' exp                                           { OpExp (pos $2) $1 TimesOp $3 }
                | exp '/' exp                                           { OpExp (pos $2) $1 DevideOp $3 }
                | exp '=' exp                                           { OpExp (pos $2) $1 EqOp $3 }
                | exp '<>' exp                                          { OpExp (pos $2) $1 NeqOp $3 }
                | exp '<' exp                                           { OpExp (pos $2) $1 LtOp $3 }
                | exp '<=' exp                                          { OpExp (pos $2) $1 LeOp $3 }
                | exp '>' exp                                           { OpExp (pos $2) $1 GtOp $3 }
                | exp '>=' exp                                          { OpExp (pos $2) $1 GeOp $3 }
                | exp '&' exp                                           { IfExp (pos $2) $1 $3 (Just $ IntExp 0) } -- A & B ≡ (A -> B) | (¬A -> ⊥) ≡ if A then B else 0
                | exp '|' exp                                           { IfExp (pos $2) $1 (IntExp 1) (Just $3) } -- A | B ≡ (A -> ⊤) | (¬A -> B) ≡ if A then 1 else B
                | varid '{' rcd '}'                                     { RecordExp (snd $1) $3 (fst $1) }
                | varid '[' exp ']' 'of' exp                            { ArrayExp (snd $1) (fst $1) $3 $6 }
                | lvalue ':=' exp                                       { AssignExp (pos $2) $1 $3 }
                | 'if' exp 'then' exp 'else' exp                        { IfExp (pos $1) $2 $4 (Just $6) }
                | 'if' exp 'then' exp                                   { IfExp (pos $1) $2 $4 Nothing }
                | 'while' exp 'do' exp                                  { WhileExp (pos $1) $2 $4 }
                | 'for' id ':=' exp 'to' exp 'do' exp                   {% do { esc <- escape; return (ForExp (pos $1) (fst $2) esc $4 $6 $8) } }
                | 'break'                                               { BreakExp (pos $1) }
                | 'let' decs 'in' seqexp 'end'                          { LetExp (pos $1) $2 $4 }

tyid          :: { (Name, Pos) }
                : id                                                    { (fst $1, pos (snd $1)) }

varid           :: { (Name, Pos) }
                : id                                                    { (fst $1, pos (snd $1)) }

seqexp          :: { Exp }
                : exps                                                  { SeqExp $1 }

exps            :: { [Exp] }
                : exp ';' exps                                          { $1 : $3 }
                | exp                                                   { [$1] }
                | {- empty -}                                           { [] }

args            :: { [Exp] }
                : exp ',' args                                          { $1 : $3 }
                | exp                                                   { [$1] }
                | {- empty -}                                           { [] }

rcd             :: { [(Name, Exp, Pos)] }
                : varid '=' exp ',' rcd                                 { (fst $1, $3, snd $1) : $5 }
                | varid '=' exp                                         { [(fst $1, $3, snd $1)] }
                | {- empty -}                                           { [] }

{
parseError :: [Token] -> IO a
parseError [] = error "parse error at EOF"
parseError (t : _) = error $ "parse error at " ++ show t

pos :: AlexPosn -> Pos
pos (AlexPn _ l c) = Pos l c

escape :: IO Escape
escape = do
        ref <- newIORef True
        return $ Escape ref
}