module Error where

data ErrorMsg = ErrorMsg
    { anyErrors :: Bool
    , fileName :: String
    , lineNum :: Int
    , linePos :: [Int]
    }