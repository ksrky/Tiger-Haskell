module SemantSpec where

import Semant.Env
import Semant.Semant
import Semant.Translate
import Syntax.Lexer
import Syntax.Parser
import Temp.Temp

import Data.Either (isLeft)
import System.IO (
        IOMode (ReadMode),
        hClose,
        hGetContents,
        openFile,
 )
import Test.Hspec (Spec, SpecWith, describe, it, shouldBe)

errorList :: [Int]
errorList = [9 .. 11] ++ [13 .. 26] ++ [31 .. 36] ++ [38 .. 40] ++ [28, 29, 43, 45, 49]

loop :: [Int] -> SpecWith ()
loop [] = return ()
loop (n : ns) = do
        let filename = "test" ++ show n
        it filename $ do
                inhandle <- openFile ("testcases/" ++ filename ++ ".tig") ReadMode
                inp <- hGetContents inhandle
                case runAlex inp parse of
                        Left err -> print err
                        Right exp -> do
                                let res = transExp (SS baseVEnv baseTEnv Outermost emptyState) exp
                                print res
                                if n `elem` errorList
                                        then isLeft res `shouldBe` True
                                        else isLeft res `shouldBe` False
        loop ns

spec :: Spec
spec = do
        describe "file test" $ do
                loop [1 .. 49]