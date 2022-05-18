module Main where

import Common.Temp
import Semant.Env
import Semant.FindEscape
import Semant.Semant
import Semant.Translate
import Syntax.Lexer
import Syntax.Parser

import Control.Monad.Trans

import qualified Compiler.Assem as Assem
import Compiler.Codegen
import Frame.Frame
import qualified Frame.Frame as Frame
import qualified IR.Canon as Canon
import System.Console.Haskeline
import System.Environment

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
                        Just "" -> outputStrLn "Goodbye."
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
        Right exp -> do
                let exp' = findEscape exp
                case transExp st exp' of
                        Left err -> print err
                        Right (ExpTy expr _) -> print expr

compile :: String -> IO ()
compile input = case runAlex input parse of
        Left err -> putStrLn err
        Right exp -> do
                let exp' = findEscape exp
                case transProg exp' of
                        Left err -> print err
                        Right expr -> print expr

{-}
emitproc out (PROC body frame) = do
        let _ = print ("emit " ^ Frame.name frame ^ "\n")
        --         val _ = Printtree.printtree(out,body); *)
        stms <- Canon.linearize body
        --         val _ = app (fn s => Printtree.printtree(out,s)) stms; *)
        stms' <- Canon.traceSchedule (Canon.basicBlocks stms)
        instrs <- concat <$> mapM (codegen frame) stms'
        let format0 = Assem.format show
        return $ map (\i -> print (out, format0 i)) instrs
emitproc out (STRING lab s) = print (out, show (lab, s))
-}