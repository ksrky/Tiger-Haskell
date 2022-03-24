module Lexer.Test where

import Lexer (Token, alexScanTokens)
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
        minput <- getInputLine "tiger-lexer> "
        case minput of
                Nothing -> return ()
                Just input -> outputStrLn $ show $ alexScanTokens input
