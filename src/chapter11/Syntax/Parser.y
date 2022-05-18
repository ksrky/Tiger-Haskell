{
module Syntax.Parser where

import Syntax.Token
import Syntax.Lexer
import qualified Syntax.Absyn as A
import qualified Common.Symbol as S
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
'/'                     { TokSymbol (SymDivide, $$) }
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

program : exp   { $1 }

decs :: { [A.Dec] }
    : dec decs          { $1 : $2 }
    | {- empty -}       { [] }

dec :: { A.Dec }
    : tydec     { $1 }
    | vardec    { $1 }
    | fundec    { $1 }

tydec :: { A.Dec }
    : 'type' id '=' ty      { A.TypeDec (id2symbol $2) $4 (pos $1) }

ty  :: { A.Ty }
    : id                    { A.NameTy (id2symbol $1) (pos $ snd $1) }
    | '{' tyfields '}'      { A.RecordTy $2 }
    | 'array' 'of' id       { A.ArrayTy (id2symbol $3) (pos $1) }

tyfields :: { [A.Field] }
    : id ':' id tyfields_       { A.Field (id2symbol $1) True (id2symbol $3) (pos $ snd $1) : $4 }
    | {- empty -}               { [] }

tyfields_ :: { [A.Field] }
    : ',' id ':' id tyfields_       { A.Field (id2symbol $2) True (id2symbol $4) (pos $1) : $5 }
    | {- empty -}                   { [] }

vardec :: { A.Dec }
    : 'var' id ':=' exp               { A.VarDec (id2symbol $2) True Nothing $4 (pos $1) }
    | 'var' id ':' id ':=' exp        { A.VarDec (id2symbol $2) True (Just (id2symbol $4, pos $ snd $4)) $6 (pos $1) }

fundec :: { A.Dec }
    : 'function' id  '(' tyfields ')' '=' exp             { A.FunDec (id2symbol $2) $4 Nothing $7 (pos $1) }
    | 'function' id '(' tyfields ')' ':' id '=' exp       { A.FunDec (id2symbol $2) $4 (Just (id2symbol $7, pos $ snd $7)) $9 (pos $1) }

lvalue :: { A.Var }
    : id                            { A.SimpleVar (id2symbol $1) (pos $ snd $1) }
    | id '.' id                     { A.FieldVar (A.SimpleVar (id2symbol $1) (pos $ snd $1)) (id2symbol $3) (pos $2) }
    | lvalue '.' id                 { A.FieldVar $1 (id2symbol $3) (pos $2) }
    | id '[' exp ']'                { A.SubscriptVar (A.SimpleVar (id2symbol $1) (pos $ snd $1)) $3 (pos $2) }
    | lvalue '[' exp ']'            { A.SubscriptVar $1 $3 (pos $2) }

exp :: { A.Exp }
    : lvalue                                { A.VarExp $1 }
    | 'nil'                                 { A.NilExp }
    | '(' seqexp ')'                        { A.SeqExp $2 (pos $1) }
    | int                                   { A.IntExp (fst $1) }
    | string                                { A.StringExp (fst $1, pos (snd $1)) }
    | '-' exp %prec UMINUS                  { A.OpExp (A.IntExp 0) A.MinusOp $2 (pos $1) }
    | id '(' args ')'                       { A.CallExp (id2symbol $1) $3 (pos $ snd $1) }
    | exp '+' exp                           { A.OpExp $1 A.PlusOp $3 (pos $2) }
    | exp '-' exp                           { A.OpExp $1 A.MinusOp $3 (pos $2) }
    | exp '*' exp                           { A.OpExp $1 A.TimesOp $3 (pos $2) }
    | exp '/' exp                           { A.OpExp $1 A.DivideOp $3 (pos $2) }
    | exp '=' exp                           { A.OpExp $1 A.EqOp $3 (pos $2) }
    | exp '<>' exp                          { A.OpExp $1 A.NeqOp $3 (pos $2) }
    | exp '<' exp                           { A.OpExp $1 A.LtOp $3 (pos $2) }
    | exp '<=' exp                          { A.OpExp $1 A.LeOp $3 (pos $2) }
    | exp '>' exp                           { A.OpExp $1 A.GtOp $3 (pos $2) }
    | exp '>=' exp                          { A.OpExp $1 A.GeOp $3 (pos $2) }
    | exp '&' exp                           { A.IfExp $1 $3 (Just $ A.IntExp 0) (pos $2) }
    | exp '|' exp                           { A.IfExp $1 (A.IntExp 1) (Just $3) (pos $2) }
    | id '{' rcd '}'                        { A.RecordExp $3 (id2symbol $1) (pos $ snd $1) }
    | id '[' exp ']' 'of' exp               { A.ArrayExp (id2symbol $1) $3 $6 (pos $ snd $1) }
    | lvalue ':=' exp                       { A.AssignExp $1 $3 (pos $2) }
    | 'if' exp 'then' exp 'else' exp        { A.IfExp $2 $4 (Just $6) (pos $1) }
    | 'if' exp 'then' exp                   { A.IfExp $2 $4 Nothing (pos $1) }
    | 'while' exp 'do' exp                  { A.WhileExp $2 $4 (pos $1) }
    | 'for' id ':=' exp 'to' exp 'do' exp   { A.ForExp (id2symbol $2) True $4 $6 $8 (pos $1) }
    | 'break'                               { A.BreakExp (pos $1) }
    | 'let' decs 'in' seqexp 'end'          { A.LetExp $2 (A.SeqExp $4 (pos $3)) (pos $1) }

seqexp :: { [A.Exp] }
    : exp seqexp_               { $1 : $2 }
    | {- empty -}               { [] }
    
seqexp_ :: { [A.Exp] }
    : ';' exp seqexp_           { $2 : $3 }
    | {- empty -}               { [] }

args :: { [A.Exp] }
    : exp args_                 { $1 : $2 }
    | {- empty -}               { [] }

args_ :: { [A.Exp] }
    : ',' exp args_             { $2 : $3 }
    | {- empty -}               { [] }

rcd :: { [(S.Symbol, A.Exp, A.Pos)] }
    : id '=' exp rcd_           { concatRcd $1 $3 $4 }
    | {- empty -}               { [] }

rcd_ :: { [(S.Symbol, A.Exp, A.Pos)] }
    : ',' id '=' exp rcd_       { concatRcd $2 $4 $5 }
    | {- empty -}               { [] }


{
parseError :: Token -> Alex a
parseError t = alexError $ "Parse error: " ++ show t

pos :: AlexPosn -> A.Pos
pos (AlexPn _ l c) = A.Pos l c

id2symbol :: (String, AlexPosn) -> S.Symbol
id2symbol (s, _) = S.symbol s

concatRcd :: (String, AlexPosn) -> A.Exp -> [(S.Symbol, A.Exp, A.Pos)] -> [(S.Symbol, A.Exp, A.Pos)]
concatRcd i e [] = [(id2symbol i, e, pos $ snd i)]
concatRcd i e r = (id2symbol i, e, pos $ snd i):r
}