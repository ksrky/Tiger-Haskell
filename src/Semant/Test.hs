module Semant.Test where

import Env (baseTEnv, baseVEnv)
import Lexer (alexScanTokens)
import Parser (parse)
import Semant (ST (ST), transExp)
import Temp (initState)
import Translate (Level (Outermost))

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
                        handle <-
                                let filename =
                                        if take 4 (reverse input) == reverse ".tig"
                                                then input
                                                else input ++ ".tig"
                                 in openFile ("testcases/" ++ filename) ReadMode
                        contents <- hGetContents handle
                        let absyn = parse $ alexScanTokens contents
                         in print (transExp (ST baseVEnv baseTEnv Outermost initState) absyn)
                        hClose handle
