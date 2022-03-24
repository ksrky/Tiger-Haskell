module Semant.Test where

import Env (baseTEnv, baseVEnv)
import Lexer (alexScanTokens)
import Parser (parse)
import Semant (transExp)

import Control.Monad.Trans.Class
import System.Console.Haskeline
import System.IO

main :: IO ()
main = do
        runInputT defaultSettings repl
        main

repl :: InputT IO ()
repl = do
        minput <- getInputLine "tiger-semant> "
        case minput of
                Nothing -> return ()
                Just input -> lift $ do
                        handle <- openFile input ReadMode
                        contents <- hGetContents handle
                        --putStrLn contents
                        let absyn = parse $ alexScanTokens contents
                         in print (transExp (baseVEnv, baseTEnv, absyn))
                        hClose handle
