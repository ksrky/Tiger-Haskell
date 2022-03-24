module Parser.Test where

import Control.Monad.Trans.Class (MonadTrans (lift))
import Lexer (Token, alexScanTokens)
import Parser (parse)
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
        minput <- getInputLine "[tiger-parser]file name here: "
        case minput of
                Nothing -> return ()
                Just input -> lift $ do
                        handle <- openFile ("testcases/" ++ input) ReadMode
                        contents <- hGetContents handle
                        print $ parse $ alexScanTokens contents
                        hClose handle