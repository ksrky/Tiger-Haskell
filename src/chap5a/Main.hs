module Main where

import Syntax.Lexer
import Syntax.Parser

import Control.Monad.Trans

import System.Console.Haskeline
import System.Environment

main :: IO ()
main = do
        args <- getArgs
        case args of
                [] -> repl
                "all" : _ -> mapM_ processFile [1 .. 49]
                nums -> mapM_ (processFile . read) nums

repl :: IO ()
repl = runInputT defaultSettings loop
    where
        loop = do
                minput <- getInputLine ">> "
                case minput of
                        Nothing -> outputStrLn "Goodbye."
                        Just "" -> outputStrLn "Goodbye."
                        Just input -> do
                                liftIO $ process input
                                loop

processFile :: Int -> IO ()
processFile num = do
        let src = "testcases/test" ++ show num ++ ".tig"
        contents <- readFile src
        putStrLn $ "----------" ++ src ++ "----------"
        process contents
        putStrLn ""

process :: String -> IO ()
process input = do
        exp <- parse $ alexScanTokens input
        print exp