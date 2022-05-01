module Error.Error where

import qualified Syntax.Absyn as A

data Error = Error {message :: String, kind :: ErrorKind, position :: A.Pos}

instance Show Error where
        show (Error msg kind pos) = show kind ++ msg ++ " at " ++ show pos

data ErrorKind
        = UnknownIdentifier {ident :: String}
        | TypeMismatch {expected :: String, got :: String}
        | TypeNotFound {typ :: String}
        | SizeMismatch
        | WrongType {required :: String, got :: String}
        | CyclicDefinition {typ :: String}
        | ImperfectDefinition {typ :: String}
        | MultipleDeclarations {name :: String}
        | RecordFieldNotFound {name :: String}
        | InvalidComparison {ltyp :: String, rtyp :: String, op :: String}
        deriving (Eq)

instance Show ErrorKind where
        show (UnknownIdentifier ident) = "unknown identifier: " ++ ident
        show (TypeMismatch expected got) = "type mismatch: expected " ++ expected ++ ", but got " ++ got
        show (TypeNotFound typ) = "type not found: " ++ typ
        show SizeMismatch = ""
        show (WrongType required got) = "wrong type: required " ++ required ++ ", but got " ++ got
        show (CyclicDefinition typ) = "recursive types interupted: " ++ typ
        show (ImperfectDefinition typ) = "imperfect type definition: " ++ typ
        show (MultipleDeclarations name) = "multiple declarations: " ++ name
        show (RecordFieldNotFound name) = "field not found: " ++ name
        show (InvalidComparison ltyp rtyp op) = "comparison of incompatible types: " ++ ltyp ++ " and " ++ rtyp ++ " with " ++ op

returnErr :: String -> ErrorKind -> A.Pos -> Either Error a
returnErr s k p = Left $ Error s k p

returnErr_ :: ErrorKind -> A.Pos -> Either Error a
returnErr_ = returnErr ""
