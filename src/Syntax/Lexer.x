{
module Syntax.Lexer where

import Syntax.Token
}

%wrapper "monad"

$digit = 0-9
$alpha = [a-zA-Z]

@id = $alpha [$alpha $digit \_ \']*
@string = \" ($printable # \")* \"
@decimal = $digit+

@reservedid = while | for | to | break | let | in | end | function | var
            | type | array | if | then | else | do | of | nil 
@reservedop = \+ | \- | \* | \/

tokens :-

<0,comment> $white+             ;
<0> "//".*                      ;
<0,comment> "/*"                { begin comment }
<comment> [^$white]*"*/"        { begin 0 }
<comment> [^$white]+            ;

<0> while           { keyword KwWhile }
<0> for             { keyword KwFor }
<0> to              { keyword KwTo }
<0> break           { keyword KwBreak }
<0> let             { keyword KwLet }
<0> in              { keyword KwIn }
<0> end             { keyword KwEnd }
<0> function        { keyword KwFunction }
<0> var             { keyword KwVar }
<0> type            { keyword KwType }
<0> array           { keyword KwArray }
<0> if              { keyword KwIf }
<0> then            { keyword KwThen }
<0> else            { keyword KwElse }
<0> do              { keyword KwDo }
<0> of              { keyword KwOf }
<0> nil             { keyword KwNil }

<0> \,              { symbol SymComma }
<0> \:              { symbol SymColon }
<0> \;              { symbol SymSemi }
<0> \(              { symbol SymLParen }
<0> \)              { symbol SymRParen }
<0> \[              { symbol SymLBrack }
<0> \]              { symbol SymRBrack }
<0> \{              { symbol SymLBrace }
<0> \}              { symbol SymRBrace }
<0> \.              { symbol SymDot }
<0> \+              { symbol SymPlus }
<0> \-              { symbol SymMinus }
<0> \*              { symbol SymTimes }
<0> \/              { symbol SymDivide }
<0> \=              { symbol SymEq }
<0> \<\>            { symbol SymNeq }
<0> \<              { symbol SymLt }
<0> \<=             { symbol SymLe }
<0> \>              { symbol SymGt }
<0> \>=             { symbol SymGe }
<0> \&              { symbol SymAnd }
<0> \|              { symbol SymOr }
<0> \:=             { symbol SymAssign }

<0> @id             { ident }
<0> @decimal        { int }
<0> @string         { string }

{
lexer :: (Token -> Alex a) -> Alex a
lexer = (alexMonadScan >>=)

type Action = AlexInput -> Int -> Alex Token

data Token
    = TokKeyword (Keyword, AlexPosn)
    | TokSymbol (Symbol, AlexPosn)
    | TokId (String, AlexPosn)
    | TokInt (Int, AlexPosn)
    | TokString (String, AlexPosn)
    | TokEof
    deriving (Eq, Show)

keyword :: Keyword -> Action
keyword key = \(pos,_,_,_) _ -> return $ TokKeyword (key, pos)

symbol :: Symbol -> Action
symbol sym = \(pos,_,_,_) _ -> return $ TokSymbol (sym, pos)

ident :: Action
ident = \(pos,_,_,str) len -> return $ TokId (take len str, pos)

int :: Action
int = \(pos,_,_,str) len -> return $ TokInt (read $ take len str, pos)

string :: Action
string = \(pos,_,_,str) len -> return $ TokString (unquot $ take len str, pos)
    where
        unquot :: String -> String
        unquot s = init (tail s)

alexEOF :: Alex Token
alexEOF = return TokEof
}