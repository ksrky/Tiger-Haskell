module Parser.Test where

import Absyn (Exp)
import Lexer (alexScanTokens)
import Parser (parse)

main :: IO ()
main = do
    s <- getLine
    print $ parse $ alexScanTokens s
    main
