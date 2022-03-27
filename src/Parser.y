-- see more info about happy from https://sites.google.com/site/paclearner/happy_jp

{
module Parser where

import Lexer
import Absyn as A
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    WHILE                                   { TkWhile $$ }
    FOR                                     { TkFor $$ }
    TO                                      { TkTo _ }
    BREAK                                   { TkBreak $$ }
    LET                                     { TkLet $$ }
    IN                                      { TkIn $$ }
    END                                     { TkEnd _ }
    FUNCTION                                { TkFunction $$ }
    VAR                                     { TkVar $$ }
    TYPE                                    { TkType $$ }
    ARRAY                                   { TkArray $$ }
    IF                                      { TkIf $$ }
    THEN                                    { TkThen _ }
    ELSE                                    { TkElse _ }
    DO                                      { TkDo _ }
    OF                                      { TkOf _ }
    NIL                                     { TkNil _ }

    ','                                     { TkComma $$ }
    ':'                                     { TkColon _ }
    ';'                                     { TkSemicolon $$ }
    '('                                     { TkLParen $$ }
    ')'                                     { TkRParen _ }
    '['                                     { TkLBrack $$ }
    ']'                                     { TkRBrack _ }
    '{'                                     { TkLBrace _ }
    '}'                                     { TkRBrace _ }
    '.'                                     { TkDot $$ }
    '+'                                     { TkPlus $$ }
    '-'                                     { TkMinus $$ }
    '*'                                     { TkTimes $$ }
    '/'                                     { TkDevide $$ }
    '='                                     { TkEQ $$ }
    '<>'                                    { TkNEQ $$ }
    '<'                                     { TkLT $$ }
    '<='                                    { TkLE $$ }
    '>'                                     { TkGT $$ }
    '>='                                    { TkGE $$ }
    '&'                                     { TkAnd $$ }
    '|'                                     { TkOr $$ }
    ':='                                    { TkAssign $$ }
    
    id                                      { TkId $$ }
    int                                     { TkInt $$ }
    string                                  { TkString $$ }

%left '+' '-'
%left '*' '/'
%left UMINUS
%left '|' '&'
%nonassoc '<' '<=' '>' '>='

%%

program : exp   { $1 }

decs :: { [Dec] }
    : dec decs      { $1 : $2 }
    | {- empty -}   { [] }

dec :: { Dec }
    : tydec     { $1 }
    | vardec    { $1 }
    | fundec    { $1 }

tydec :: { Dec }
    : TYPE typeid '=' ty    { A.TypeDec (fst $2) $4 (pos $1) }

ty  :: { Ty }
    : typeid                { A.NameTy (fst $1) (pos $ snd $1) }
    | '{' tyfields '}'      { A.RecordTy $2 }
    | ARRAY OF typeid       { A.ArrayTy (fst $3) (pos $1) }

tyfields :: { [Field] }
    : id ':' typeid tyfields_   { A.Field (fst $1) True (fst $3) (pos $ snd $1) : $4 }
    | {- empty -}               { [] }

tyfields_ :: { [Field] }
    : ',' id ':' typeid tyfields_   { A.Field (fst $2) True (fst $4) (pos $1) : $5 }
    | {- empty -}                   { [] }

vardec :: { Dec }
    : VAR id ':=' exp               { A.VarDec (fst $2) True Nothing $4 (pos $1) }
    | VAR id ':' typeid ':=' exp    { A.VarDec (fst $2) True (Just (fst $4, pos $ snd $4)) $6 (pos $1) }

fundec :: { Dec }
    : FUNCTION id  '(' tyfields ')' '=' exp             { A.FunDec (fst $2) $4 Nothing $7 (pos $1) }
    | FUNCTION id '(' tyfields ')' ':' typeid '=' exp   { A.FunDec (fst $2) $4 (Just (fst $7, pos $ snd $7)) $9 (pos $1) }

lvalue :: { Var }
    : id                            { A.SimpleVar (fst $1) (pos $ snd $1) }
    | lvalue '.' id                 { A.FieldVar $1 (fst $3) (pos $2) }
    | lvalue '[' exp ']'            { A.SubscriptVar $1 $3 (pos $2) }

exp :: { Exp }
    : lvalue                                { A.VarExp $1 }
    | NIL                                   { A.NilExp }
    | '(' exp ';' exp seqexp_ ')'           { A.SeqExp (concatSeqexp $2 (concatSeqexp $4 $5), pos $1) }
    | int                                   { A.IntExp (fst $1) }
    | string                                { A.StringExp (fst $1, pos (snd $1)) }
    | '-' exp %prec UMINUS                  { A.OpExp (A.IntExp 0) A.MinusOp $2 (pos $1) }
    | id '(' args ')'                       { A.CallExp (fst $1) $3 (pos $ snd $1) }
    | exp '+' exp                           { A.OpExp $1 A.PlusOp $3 (pos $2) }
    | exp '-' exp                           { A.OpExp $1 A.MinusOp $3 (pos $2) }
    | exp '*' exp                           { A.OpExp $1 A.TimesOp $3 (pos $2) }
    | exp '/' exp                           { A.OpExp $1 A.DevideOp $3 (pos $2) }
    | exp '=' exp                           { A.OpExp $1 A.EqOp $3 (pos $2) }
    | exp '<>' exp                          { A.OpExp $1 A.NeqOp $3 (pos $2) }
    | exp '<' exp                           { A.OpExp $1 A.LtOp $3 (pos $2) }
    | exp '<=' exp                          { A.OpExp $1 A.LeOp $3 (pos $2) }
    | exp '>' exp                           { A.OpExp $1 A.GtOp $3 (pos $2) }
    | exp '>=' exp                          { A.OpExp $1 A.GeOp $3 (pos $2) }
    | exp '&' exp                           { A.IfExp $1 $3 (Just $ A.IntExp 0) (pos $2) } -- A & B ≡ (A -> B) | (¬A -> ⊥) ≡ if A then B else 0
    | exp '|' exp                           { A.IfExp $1 (A.IntExp 1) (Just $3) (pos $2) } -- A | B ≡ (A -> ⊤) | (¬A -> B) ≡ if A then 1 else B
    | id '{' rcd '}'                        { A.RecordExp $3 (fst $1) (pos $ snd $1) }
    | id '[' exp ']' OF exp                 { A.ArrayExp (fst $1) $3 $6 (pos $ snd $1) }
    | lvalue ':=' exp                       { A.AssignExp $1 $3 (pos $2) }
    | IF exp THEN exp ELSE exp              { A.IfExp $2 $4 (Just $6) (pos $1) }
    | IF exp THEN exp                       { A.IfExp $2 $4 Nothing (pos $1) }
    | WHILE '(' exp ')' DO exp              { A.WhileExp $3 $6 (pos $1) }
    | FOR id ':=' exp TO exp DO exp         { A.ForExp (fst $2) True $4 $6 $8 (pos $1) }
    | BREAK                                 { A.BreakExp (pos $1) }
    | LET decs IN seqexp END                { A.LetExp $2 (A.SeqExp ($4, pos $3)) (pos $1) }

typeid :: { (Symbol, AlexPosn) }
    : id    { $1 }

seqexp :: { [Exp] }
    : exp seqexp_               { concatSeqexp $1 $2 }
    | {- empty -}               { [] }
    
seqexp_ :: { [Exp] }
    : ';' exp seqexp_           { concatSeqexp $2 $3 }
    | {- empty -}               { [] }

args :: { [Exp] }
    : exp args_                 { $1 : $2 }
    | {- empty -}               { [] }

args_ :: { [Exp] }
    : ',' exp args_             { $2 : $3 }
    | {- empty -}               { [] }

rcd :: { [(Symbol, Exp, Pos)] }
    : id '=' exp rcd_           { concatRcd $1 $3 $4 }
    | {- empty -}               { [] }

rcd_ :: { [(Symbol, Exp, Pos)] }
    : ',' id '=' exp rcd_       { concatRcd $2 $4 $5 }
    | {- empty -}               { [] }


{
parseError :: [Token] -> a
parseError [] = error "Parse error at EOF"
parseError (t:_) = error $ "Parse error of " ++ showToken t

pos :: AlexPosn -> (Int, Int)
pos (AlexPn _ l c) = (l, c)

concatSeqexp :: Exp ->  [Exp] -> [Exp]
concatSeqexp e [] = [e]
concatSeqexp e es = e:es

concatRcd :: (Symbol, AlexPosn) -> Exp -> [(Symbol, Exp, Pos)] -> [(Symbol, Exp, Pos)]
concatRcd i e [] = [(fst i, e, pos (snd i))]
concatRcd i e r = (fst i, e, pos (snd i)):r
}