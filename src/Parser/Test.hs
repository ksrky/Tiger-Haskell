module Parser.Test where

import Absyn (Exp)
import Lexer (alexScanTokens)
import Parser (parse)

import System.Console.Haskeline (
    InputT,
    defaultSettings,
    getInputLine,
    outputStrLn,
    runInputT,
 )

main :: IO ()
main = do
    runInputT defaultSettings repl
    main

repl :: InputT IO ()
repl = do
    minput <- getInputLine "tiger-parser> "
    case minput of
        Nothing -> return ()
        Just input -> outputStrLn $ show $ parse $ alexScanTokens input
