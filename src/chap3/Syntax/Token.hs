module Syntax.Token where

data Keyword
        = KwWhile
        | KwFor
        | KwTo
        | KwBreak
        | KwLet
        | KwIn
        | KwEnd
        | KwFunction
        | KwVar
        | KwType
        | KwArray
        | KwIf
        | KwThen
        | KwElse
        | KwDo
        | KwOf
        | KwNil
        deriving (Eq, Show)

data Symbol
        = SymComma
        | SymColon
        | SymSemi
        | SymLParen
        | SymRParen
        | SymLBrack
        | SymRBrack
        | SymLBrace
        | SymRBrace
        | SymDot
        | SymPlus
        | SymMinus
        | SymTimes
        | SymDivide
        | SymEQ
        | SymNEQ
        | SymLT
        | SymLE
        | SymGT
        | SymGE
        | SymAnd
        | SymOr
        | SymAssign
        deriving (Eq, Show)