module Main where

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
repl = runInputT defaultSettings loop
    where
        loop = do
                minput <- getInputLine ">> "
                case minput of
                        Nothing -> outputStrLn "Goodbye."
                        Just input -> do
                                liftIO $ process input
                                loop

processFile :: [Int] -> IO ()
processFile [] = return ()
processFile (n : ns) = do
        let fname = "testcases/test" ++ show n ++ ".tig"
        contents <- readFile fname
        process contents
        processFile ns

process :: String -> IO ()
process input = print $ runAlex input parse