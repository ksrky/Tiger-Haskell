module Semant.Test where

import Env (baseTEnv, baseVEnv)
import Lexer (alexScanTokens)
import Parser (parse)
import Semant (transExp)

import Control.Monad.Trans.Class (MonadTrans (lift))
import System.Console.Haskeline (
        InputT,
        defaultSettings,
        getInputLine,
        runInputT,
 )
import System.IO (
        IOMode (ReadMode),
        hClose,
        hGetContents,
        openFile,
 )

main :: IO ()
main = do
        runInputT defaultSettings test
        main

test :: InputT IO ()
test = do
        minput <- getInputLine "[tiger-semant]file name here: "
        case minput of
                Nothing -> return ()
                Just input -> lift $ do
                        handle <- openFile ("testcases/" ++ input) ReadMode
                        contents <- hGetContents handle
                        let absyn = parse $ alexScanTokens contents
                         in print (transExp (baseVEnv, baseTEnv, absyn))
                        hClose handle
