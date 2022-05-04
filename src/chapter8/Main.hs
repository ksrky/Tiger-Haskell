module Main where

import Common.Temp
import Semant.Env
import Semant.Semant
import Semant.Translate
import Syntax.Lexer
import Syntax.Parser

import Control.Monad.Trans

import System.Console.Haskeline
import System.Environment
import System.IO

main :: IO ()
main = do
        args <- getArgs
        case args of
                [] -> repl
                "all" : _ -> processFile [1 .. 49]
                ns -> processFile (map read ns)

repl :: IO ()
repl = runInputT defaultSettings (loop initState)
    where
        loop st = do
                minput <- getInputLine ">> "
                case minput of
                        Nothing -> outputStrLn "Goodbye."
                        Just input -> do
                                liftIO $ process st input
                                loop st

processFile :: [Int] -> IO ()
processFile [] = return ()
processFile (n : ns) = do
        let fname = "testcases/test" ++ show n ++ ".tig"
        contents <- readFile fname
        putStrLn $ "----------" ++ fname ++ "----------"
        process initState contents
        putStrLn ""
        processFile ns

process :: SemantState -> String -> IO ()
process st input = case runAlex input parse of
        Left err -> putStrLn err
        Right exp -> print $ transExp st exp
