-- see more info about alex from https://www.haskell.org/alex/doc/html/index.html

{
    module Lexer where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$graphic = $printable # $white 
-- $graphic matches all the characters in $printable that are not in $white.
-- $printable corresponds to Unicode code points 32 to 0x10ffff.

@id = $alpha [$alpha $digit \_ \']*
@string = \" ($printable # \")* \"

tokens :-

    $white+                                 ;
    "//".*                                  ;
    "/*".*"*/"                              ;

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

    @id		                                { \pos s -> TkId (s, pos) }
    $digit+                                 { \pos s -> TkInt ((read s), pos) }
    @string                                 { \pos s -> TkString (unquot s, pos) }

{
unquot :: String -> String
unquot s = init (tail s)

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
    | TkNEQ AlexPosn
    | TkLT AlexPosn
    | TkLE AlexPosn
    | TkGT AlexPosn
    | TkGE AlexPosn
    | TkAnd AlexPosn
    | TkOr AlexPosn
    | TkAssign AlexPosn
    
    | TkId (String, AlexPosn)
    | TkInt (Integer, AlexPosn)
    | TkString (String, AlexPosn)
    deriving (Eq, Show)

showPos (AlexPn _ l c) = " at " ++ show (l, c)

showToken :: Token -> String
showToken (TkWhile p) = "\'while\'" ++ showPos p
showToken (TkFor p) = "\'for\'" ++ showPos p
showToken (TkTo p) = "\'to\'" ++ showPos p
showToken (TkBreak p) = "\'break\'" ++ showPos p
showToken (TkLet p) = "\'let\'" ++ showPos p
showToken (TkIn p) = "\'in\'" ++ showPos p
showToken (TkEnd p) = "\'end\'" ++ showPos p
showToken (TkFunction p) = "\'function\'" ++ showPos p
showToken (TkVar p) = "\'var\'" ++ showPos p
showToken (TkType p) = "\'type\'" ++ showPos p
showToken (TkArray p) = "\'array\'" ++ showPos p
showToken (TkIf p) = "\'if\'" ++ showPos p
showToken (TkThen p) = "\'then\'" ++ showPos p
showToken (TkElse p) = "\'else\'" ++ showPos p
showToken (TkDo p) = "\'do\'" ++ showPos p
showToken (TkOf p) = "\'of\'" ++ showPos p
showToken (TkNil p) = "\'nil\'" ++ showPos p
showToken (TkComma p) = "\',\'" ++ showPos p
showToken (TkColon p) = "\':\'" ++ showPos p
showToken (TkSemicolon p) = "\';\'" ++ showPos p
showToken (TkLParen p) = "\'(\'" ++ showPos p
showToken (TkRParen p) = "\')\'" ++ showPos p
showToken (TkLBrack p) = "\'[\'" ++ showPos p
showToken (TkRBrack p) = "\']\'" ++ showPos p
showToken (TkLBrace p) = "\'{\'" ++ showPos p
showToken (TkRBrace p) = "\'}\'" ++ showPos p
showToken (TkDot p) = "\'.\'" ++ showPos p
showToken (TkPlus p) = "\'+\'" ++ showPos p
showToken (TkMinus p) = "\'-\'" ++ showPos p
showToken (TkTimes p) = "\'*\'" ++ showPos p
showToken (TkDevide p) = "\'/\'" ++ showPos p
showToken (TkEQ p) = "\'=\'" ++ showPos p
showToken (TkNEQ p) = "\'<>\'" ++ showPos p
showToken (TkLT p) = "\'<\'" ++ showPos p
showToken (TkLE p) = "\'<=\'" ++ showPos p
showToken (TkGT p) = "\'>\'" ++ showPos p
showToken (TkGE p) = "\'>=\'" ++ showPos p
showToken (TkAnd p) = "\'&\'" ++ showPos p
showToken (TkOr p) = "\'|\'" ++ showPos p
showToken (TkAssign p) = "\':=\'" ++ showPos p
showToken (TkId (s, p)) = "Id: " ++ s ++ showPos p
showToken (TkInt (i, p)) = show i ++ showPos p
showToken (TkString (s, p)) = s ++ showPos p
}