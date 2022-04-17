module Error.Error where

import qualified Syntax.Absyn as A

data Error = Error {message :: String, kind :: ErrorKind, position :: A.Pos}

instance Show Error where
        show (Error msg kind pos) = show kind ++ msg ++ " at " ++ show pos

data ErrorKind
        = ErrUnknownIdentifier {ident :: String}
        | ErrTypeMismatch {expected :: String, got :: String}
        | ErrTypeNotFound {typ :: String}
        | ErrSizeMismatch
        | ErrWrongType
        | ErrCyclicDefinition {typ :: String}
        | ErrImperfectDefinition {typ :: String}
        | ErrMultipleDeclarations {name :: String}
        | Err
        deriving (Eq)

instance Show ErrorKind where
        show (ErrUnknownIdentifier ident) = "unknown identifier: " ++ ident
        show (ErrTypeMismatch expected got) = "type mismatch: expected " ++ expected ++ ", but got " ++ got
        show (ErrTypeNotFound typ) = "type not found: " ++ typ
        show ErrSizeMismatch = ""
        show ErrWrongType = ""
        show (ErrCyclicDefinition typ) = "recursive types interupted: " ++ typ
        show (ErrImperfectDefinition typ) = "imperfect type definition: " ++ typ
        show (ErrMultipleDeclarations name) = "multiple declarations: " ++ name
        show Err = ""

returnErr :: String -> ErrorKind -> A.Pos -> Either Error a
returnErr s k p = Left $ Error s k p
