{
module Syntax.Lexer where

import Syntax.Token
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

@id = $alpha [$alpha $digit \_ \']*
@string = \" ($printable # \")* \"
@decimal = $digit+

tokens :-

$white+                 ;
"//".*                  ;
"/*" $printable* "*/"   ;

while                   { keyword KwWhile }
for                     { keyword KwFor }
to                      { keyword KwTo }
break                   { keyword KwBreak }
let                     { keyword KwLet }
in                      { keyword KwIn }
end                     { keyword KwEnd }
function                { keyword KwFunction }
var                     { keyword KwVar }
type                    { keyword KwType }
array                   { keyword KwArray }
if                      { keyword KwIf }
then                    { keyword KwThen }
else                    { keyword KwElse }
do                      { keyword KwDo }
of                      { keyword KwOf }
nil                     { keyword KwNil }

\,                      { symbol SymComma }
\:                      { symbol SymColon }
\;                      { symbol SymSemi }
\(                      { symbol SymLParen }
\)                      { symbol SymRParen }
\[                      { symbol SymLBrack }
\]                      { symbol SymRBrack }
\{                      { symbol SymLBrace }
\}                      { symbol SymRBrace }
\.                      { symbol SymDot }
\+                      { symbol SymPlus }
\-                      { symbol SymMinus }
\*                      { symbol SymTimes }
\/                      { symbol SymDivide }
\=                      { symbol SymEQ }
\<\>                    { symbol SymNEQ }
\<                      { symbol SymLT }
\<=                     { symbol SymLE }
\>                      { symbol SymGT }
\>=                     { symbol SymGE }
\&                      { symbol SymAnd }
\|                      { symbol SymOr }
\:=                     { symbol SymAssign }

@id                     { ident }
@decimal                { int }
@string                 { string }

{
type Action = AlexPosn -> String -> Token

data Token
    = TokKeyword (Keyword, AlexPosn)
    | TokSymbol (Symbol, AlexPosn)
    | TokId (String, AlexPosn)
    | TokInt (Int, AlexPosn)
    | TokString (String, AlexPosn)
    deriving (Eq, Show)

keyword :: Keyword -> Action
keyword key = \pos _ -> TokKeyword (key, pos)

symbol :: Symbol -> Action
symbol sym = \pos _ -> TokSymbol (sym, pos)

ident :: Action
ident = \pos inp -> TokId (inp, pos)

int :: Action
int = \pos inp -> TokInt (read inp, pos)

string :: Action
string = \pos inp -> TokString (init (tail inp), pos)
}