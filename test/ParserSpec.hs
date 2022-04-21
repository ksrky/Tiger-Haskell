module ParserSpec where

import Syntax.Lexer
import Syntax.Parser

import System.IO (
        IOMode (ReadMode),
        hClose,
        hGetContents,
        openFile,
 )
import Test.Hspec (Spec, SpecWith, describe, it)

loop :: [Int] -> SpecWith ()
loop [] = return ()
loop (n : ns) = do
        let filename = "test" ++ show n
        it filename $ do
                inhandle <- openFile ("testcases/" ++ filename ++ ".tig") ReadMode
                inp <- hGetContents inhandle
                print $ runAlex inp parse
        loop ns

spec :: Spec
spec = do
        describe "file test" $ do
                loop [1 .. 1]