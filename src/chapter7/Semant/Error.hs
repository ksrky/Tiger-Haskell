module Semant.Error where

import qualified Syntax.Absyn as A

data Error = Error {message :: String, kind :: ErrorKind, position :: A.Pos}

instance Show Error where
        show (Error msg kind pos) = show pos ++ ":error:\t" ++ show kind ++ msg

data ErrorKind
        = UnknownVariable {var :: String}
        | UnknownFunction {fun :: String}
        | UnknownType {typ :: String}
        | TypeMismatch {expected :: String, got :: String}
        | WrongType {required :: String, got :: String}
        | CyclicDefinition {typ :: String}
        | ImperfectDefinition {typ :: String}
        | MultipleDeclarations {name :: String}
        | InvalidComparison {ltyp :: String, rtyp :: String, op :: String}
        | Other
        deriving (Eq)

instance Show ErrorKind where
        show (UnknownVariable var) = "unknown variable: " ++ var
        show (UnknownFunction fun) = "unknown function: " ++ fun
        show (UnknownType typ) = "unknown type: " ++ typ
        show (TypeMismatch expected got) = "type mismatch: expected " ++ expected ++ ", but got " ++ got
        show (WrongType required got) = "wrong type: required " ++ required ++ ", but got " ++ got
        show (CyclicDefinition typ) = "recursive types interupted: " ++ typ
        show (ImperfectDefinition typ) = "imperfect type definition: " ++ typ
        show (MultipleDeclarations name) = "multiple declarations: " ++ name
        show (InvalidComparison ltyp rtyp op) = "comparison of incompatible types: " ++ ltyp ++ " and " ++ rtyp ++ " with " ++ op
        show Other = ""

returnErr :: String -> ErrorKind -> A.Pos -> Either Error a
returnErr s k p = Left $ Error s k p

returnErr_ :: ErrorKind -> A.Pos -> Either Error a
returnErr_ = returnErr ""

unknownVariable :: String -> A.Pos -> Either Error a
unknownVariable ident = returnErr_ $ UnknownVariable ident

unknownFunction :: String -> A.Pos -> Either Error a
unknownFunction func = returnErr_ $ UnknownFunction func

unknownType :: String -> A.Pos -> Either Error a
unknownType typ = returnErr_ $ UnknownType typ

typeMismatch :: String -> String -> A.Pos -> Either Error a
typeMismatch expected got = returnErr_ $ TypeMismatch expected got

wrongType :: String -> String -> A.Pos -> Either Error a
wrongType required got = returnErr_ $ WrongType required got

cyclicDefinition :: String -> A.Pos -> Either Error a
cyclicDefinition typ = returnErr_ $ CyclicDefinition typ

imperfectDefinition :: String -> A.Pos -> Either Error a
imperfectDefinition typ = returnErr_ $ ImperfectDefinition typ

multipleDeclarations :: String -> A.Pos -> Either Error a
multipleDeclarations name = returnErr_ $ MultipleDeclarations name

invalidComparison :: String -> String -> String -> A.Pos -> Either Error a
invalidComparison ltyp rtyp op = returnErr_ $ InvalidComparison ltyp rtyp op

otherError :: String -> A.Pos -> Either Error a
otherError msg = returnErr msg Other