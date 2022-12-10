{-# LANGUAGE OverloadedStrings #-}

module Syntax.Absyn where

import Data.IORef
import Prettyprinter

data Pos = Pos {line :: Int, col :: Int} deriving (Eq, Show)

newtype Escape = Escape {getEscape :: IORef Bool} deriving (Eq)

instance Show Escape where show _ = ""

type Name = String

data Var
        = SimpleVar Pos Name
        | FieldVar Pos Var Name
        | SubscriptVar Pos Var Exp
        deriving (Eq, Show)

data Exp
        = VarExp Var
        | NilExp
        | IntExp Int
        | StringExp Pos String
        | CallExp Pos Name [Exp]
        | OpExp Pos Exp Oper Exp
        | RecordExp Pos [(Name, Exp, Pos)] Name
        | SeqExp [Exp]
        | AssignExp Pos Var Exp
        | IfExp Pos Exp Exp (Maybe Exp)
        | WhileExp Pos Exp Exp
        | ForExp Pos Name Escape Exp Exp Exp
        | BreakExp Pos
        | LetExp Pos [Dec] Exp
        | ArrayExp Pos Name Exp Exp
        deriving (Eq, Show)

data Dec
        = FunctionDec [FunDec]
        | VarDec Pos Name Escape (Maybe (Name, Pos)) Exp
        | TypeDec [(Name, Ty, Pos)]
        deriving (Eq, Show)

data Ty
        = NameTy Pos Name
        | RecordTy [Field]
        | ArrayTy Pos Name
        deriving (Eq, Show)

data Oper
        = PlusOp
        | MinusOp
        | TimesOp
        | DivideOp
        | EqOp
        | NeqOp
        | LtOp
        | LeOp
        | GtOp
        | GeOp
        deriving (Eq, Show)

data Field = Field
        { fieldPos :: Pos
        , fieldName :: Name
        , fieldEscape :: Escape
        , fieldType :: Name
        }
        deriving (Eq, Show)

data FunDec = FunDec
        { funPos :: Pos
        , funName :: Name
        , funParams :: [Field]
        , funResult :: Maybe (Name, Pos)
        , funBody :: Exp
        }
        deriving (Eq, Show)

----------------------------------------------------------------
-- Pretty printing
----------------------------------------------------------------
instance Pretty Var where
        pretty (SimpleVar _ x) = pretty x
        pretty (FieldVar _ var x) = hcat [pretty var, dot, pretty x]
        pretty (SubscriptVar _ var exp) = hcat [pretty var, lbracket, pretty exp, rbracket]

instance Pretty Exp where
        pretty (VarExp var) = pretty var
        pretty NilExp = "nil"
        pretty (IntExp n) = pretty n
        pretty (StringExp _ str) = dquotes (pretty str)
        pretty (CallExp _ fun args) =
                hcat
                        [ pretty fun
                        , lparen
                        , concatWith (surround (comma <> space)) (map pretty args)
                        , rparen
                        ]
        pretty (OpExp _ lhs op rhs) = hsep [pretty lhs, pretty op, pretty rhs]
        pretty (RecordExp _ fields typ) =
                hcat
                        [ pretty typ
                        , lbrace
                        , concatWith (surround (comma <> space)) (map (\(n, e, _) -> hsep [pretty n, equals, pretty e]) fields)
                        , rbrace
                        ]
        pretty (SeqExp seqexp) = concatWith (surround (comma <> space)) (map pretty seqexp)
        pretty (AssignExp _ var exp) = hsep [pretty var, dot <> equals, pretty exp]
        pretty (IfExp _ test then' Nothing) =
                hsep
                        [ "if"
                        , pretty test
                        , "then"
                        , pretty then'
                        ]
        pretty (IfExp _ test then' (Just else')) =
                hsep
                        [ "if"
                        , pretty test
                        , "then"
                        , pretty then'
                        , "else"
                        , pretty else'
                        ]
        pretty (WhileExp _ test body) =
                hsep
                        [ "while"
                        , pretty test
                        , lbrace
                        , pretty body
                        , rbrace
                        ]
        pretty (ForExp _ i _ lo hi body) =
                hsep
                        [ "for"
                        , pretty i
                        , dot <> equals
                        , pretty lo
                        , "to"
                        , pretty hi
                        , "do"
                        , pretty body
                        ]
        pretty BreakExp{} = "break"
        pretty (LetExp _ decs body) =
                hsep
                        [ "let"
                        , concatWith (surround (comma <> space)) (map pretty decs)
                        , "in"
                        , pretty body
                        , "end"
                        ]
        pretty (ArrayExp _ typ size init) = hcat [pretty typ, lbracket, pretty size, rbracket, "of", pretty init]

instance Pretty Dec where
        pretty (FunctionDec fundecs) = vsep (map pretty fundecs)
        pretty (VarDec _ x _ Nothing exp) =
                vsep
                        [ "var"
                        , pretty x
                        , colon <> equals
                        , pretty exp
                        ]
        pretty (VarDec _ x _ (Just (typ, _)) exp) =
                vsep
                        [ "var"
                        , pretty x
                        , colon
                        , pretty typ
                        , colon <> equals
                        , pretty exp
                        ]
        pretty (TypeDec fields) = vsep (map (\(typ, ty, _) -> hsep ["type", pretty typ, equals, pretty ty]) fields)

instance Pretty Ty where
        pretty (NameTy _ typ) = pretty typ
        pretty (RecordTy fields) = concatWith (surround (comma <> space)) (map pretty fields)
        pretty (ArrayTy _ typ) = pretty typ

instance Pretty Oper where
        pretty PlusOp = "+"
        pretty MinusOp = "-"
        pretty TimesOp = "*"
        pretty DivideOp = "/"
        pretty EqOp = "="
        pretty NeqOp = "<>"
        pretty LtOp = "<"
        pretty LeOp = "<="
        pretty GtOp = ">"
        pretty GeOp = ">="

instance Pretty FunDec where
        pretty (FunDec _ name params Nothing body) =
                hsep
                        [ "function"
                        , pretty name
                        , lparen
                        , concatWith (surround (comma <> space)) (map pretty params)
                        , rparen
                        , equals
                        , pretty body
                        ]
        pretty (FunDec _ name params (Just (typ, _)) body) =
                hsep
                        [ "function"
                        , pretty name
                        , lparen
                        , pretty params
                        , rparen
                        , colon
                        , pretty typ
                        , equals
                        , pretty body
                        ]

instance Pretty Field where
        pretty (Field _ lab _ typ) = pretty lab <> colon <+> pretty typ