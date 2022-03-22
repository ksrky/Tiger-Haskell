-- see more info about alex: https://www.haskell.org/alex/doc/html/index.html

{
    module Lexer where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$graphic = $printable # $white 
-- $graphic matches all the characters in $printable that are not in $white.
-- z$printable corresponds to Unicode code points 32 to 0x10ffff.

@id = $alpha [$alpha $digit \_ \']*
@string     = \" ($graphic # \")* \"

tokens :-

    $white+                                 ;
    "//".*                                  ;

    while                                   { \pos _ -> TkWhile pos }
    for                                     { \pos _ -> TkFor pos }
    to                                      { \pos _ -> TkTo pos }
    break                                   { \pos _ -> TkBreak pos }
    let                                     { \pos _ -> TkLet pos }
    in                                      { \pos _ -> TkIn pos }
    end                                     { \pos _ -> TkEnd pos }
    function                                { \pos _ -> TkFunction pos }
    var                                     { \pos _ -> TkVar pos }
    type                                    { \pos _ -> TkType pos }
    array                                   { \pos _ -> TkArray pos }
    if                                      { \pos _ -> TkIf pos }
    then                                    { \pos _ -> TkThen pos }
    else                                    { \pos _ -> TkElse pos }
    do                                      { \pos _ -> TkDo pos }
    of                                      { \pos _ -> TkOf pos }
    nil                                     { \pos _ -> TkNil pos }

    \,                                      { \pos _ -> TkComma pos }
    \:                                      { \pos _ -> TkColon pos }
    \;                                      { \pos _ -> TkSemicolon pos }
    \(                                      { \pos _ -> TkLParen pos }
    \)                                      { \pos _ -> TkRParen pos }
    \[                                      { \pos _ -> TkLBrack pos }
    \]                                      { \pos _ -> TkRBrack pos }
    \{                                      { \pos _ -> TkLBrace pos }
    \}                                      { \pos _ -> TkRBrace pos }
    \.                                      { \pos _ -> TkDot pos }
    \+                                      { \pos _ -> TkPlus pos }
    \-                                      { \pos _ -> TkMinus pos }
    \*                                      { \pos _ -> TkTimes pos }
    \/                                      { \pos _ -> TkDevide pos }
    \=                                      { \pos _ -> TkEQ pos }
    \<\>                                    { \pos _ -> TkNEQ pos }
    \<                                      { \pos _ -> TkLT pos }
    \<=                                     { \pos _ -> TkLE pos }
    \>                                      { \pos _ -> TkGT pos }
    \>=                                     { \pos _ -> TkGE pos }
    \&                                      { \pos _ -> TkAnd pos }
    \|                                      { \pos _ -> TkOr pos }
    \:=                                     { \pos _ -> TkAssign pos }

    @string                                 { \pos s -> TkString (s, pos) }
    @id		                                { \pos s -> TkId (s, pos) }
    $digit+                                 { \pos s -> TkInt ((read s), pos) }

{
    data Token
        = TkWhile AlexPosn
        | TkFor AlexPosn
        | TkTo AlexPosn
        | TkBreak AlexPosn
        | TkLet AlexPosn
        | TkIn AlexPosn
        | TkEnd AlexPosn
        | TkFunction AlexPosn
        | TkVar AlexPosn
        | TkType AlexPosn
        | TkArray AlexPosn
        | TkIf AlexPosn
        | TkThen AlexPosn
        | TkElse AlexPosn
        | TkDo AlexPosn
        | TkOf AlexPosn
        | TkNil AlexPosn

        | TkComma AlexPosn
        | TkColon AlexPosn
        | TkSemicolon AlexPosn
        | TkLParen AlexPosn
        | TkRParen AlexPosn
        | TkLBrack AlexPosn
        | TkRBrack AlexPosn
        | TkLBrace AlexPosn
        | TkRBrace AlexPosn
        | TkDot AlexPosn
        | TkPlus AlexPosn
        | TkMinus AlexPosn
        | TkTimes AlexPosn
        | TkDevide AlexPosn
        | TkEQ AlexPosn
        | TkAnd AlexPosn
        | TkLT AlexPosn
        | TkLE AlexPosn
        | TkGT AlexPosn
        | TkGE AlexPosn
        | TkNEQ AlexPosn
        | TkOr AlexPosn
        | TkAssign AlexPosn
        
        | TkId (String, AlexPosn)
        | TkInt (Integer, AlexPosn)
        | TkString (String, AlexPosn)
        deriving (Eq, Show)
}