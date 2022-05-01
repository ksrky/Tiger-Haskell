module Main where

import Semant.Env
import Semant.Semant
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
                ns -> processFile (map read ns)

repl :: IO ()
repl = runInputT defaultSettings (loop (SS baseVEnv baseTEnv))
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
        process (SS baseVEnv baseTEnv) contents
        processFile ns

process :: SemantState -> String -> IO ()
process st input = case runAlex input parse of
        Left err -> print err
        Right exp -> print $ transExp st exp
