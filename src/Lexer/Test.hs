module Lexer.Test where

import Lexer (Token, alexScanTokens)

main :: IO ()
main = do
    s <- getLine
    print (alexScanTokens s)
    main
