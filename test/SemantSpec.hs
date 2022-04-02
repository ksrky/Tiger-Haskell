module SemantSpec where

import Env
import Lexer
import Parser
import Semant
import Temp (initState)
import Translate
import qualified Types as T

import System.IO
import Test.Hspec

spec :: Spec
spec = do
        describe "test1.tig" $ do
                it "an array type and an array variable" $ do
                        handle <- openFile "testcases/test1.tig" ReadMode
                        contents <- hGetContents handle
                        let absyn = parse $ alexScanTokens contents
                         in show (transExp (ST baseVEnv baseTEnv Outermost initState) absyn) `shouldBe` show (T.ARRAY T.INT)
        describe "test2.tig" $ do
                it "arr1 is valid since expression 0 is int = myint" $ do
                        handle <- openFile "testcases/test2.tig" ReadMode
                        contents <- hGetContents handle
                        let absyn = parse $ alexScanTokens contents
                         in show (transExp (ST baseVEnv baseTEnv Outermost initState) absyn) `shouldBe` show (T.ARRAY T.INT)
        describe "test3.tig" $ do
                it "a record type and a record variable" $ do
                        handle <- openFile "testcases/test3.tig" ReadMode
                        contents <- hGetContents handle
                        let absyn = parse $ alexScanTokens contents
                         in show (transExp (ST baseVEnv baseTEnv Outermost initState) absyn) `shouldBe` show (T.RECORD [("name", T.STRING), ("age", T.INT)])
        describe "test4.tig" $ do
                it "define a recursive function" $ do
                        handle <- openFile "testcases/test4.tig" ReadMode
                        contents <- hGetContents handle
                        let absyn = parse $ alexScanTokens contents
                         in show (transExp (ST baseVEnv baseTEnv Outermost initState) absyn) `shouldBe` show T.INT
        describe "test5.tig" $ do
                it "define valid recursive types" $ do
                        handle <- openFile "testcases/test5.tig" ReadMode
                        contents <- hGetContents handle
                        let absyn = parse $ alexScanTokens contents
                         in show (transExp (ST baseVEnv baseTEnv Outermost initState) absyn) `shouldBe` show (T.RECORD [("hd", T.INT), ("tl", T.NAME "intlist" Nothing)])
        describe "test6.tig" $ do
                it "define valid mutually recursive procedures" $ do
                        handle <- openFile "testcases/test6.tig" ReadMode
                        contents <- hGetContents handle
                        let absyn = parse $ alexScanTokens contents
                         in show (transExp (ST baseVEnv baseTEnv Outermost initState) absyn) `shouldBe` show T.UNIT
        describe "test7.tig" $ do
                it "define valid mutually recursive functions" $ do
                        handle <- openFile "testcases/test7.tig" ReadMode
                        contents <- hGetContents handle
                        let absyn = parse $ alexScanTokens contents
                         in show (transExp (ST baseVEnv baseTEnv Outermost initState) absyn) `shouldBe` show T.INT
        describe "test8.tig" $ do
                it "correct if" $ do
                        handle <- openFile "testcases/test8.tig" ReadMode
                        contents <- hGetContents handle
                        let absyn = parse $ alexScanTokens contents
                         in show (transExp (ST baseVEnv baseTEnv Outermost initState) absyn) `shouldBe` show T.INT
        describe "test9.tig" $ do
                it "error : types of then - else differ" $ do
                        handle <- openFile "testcases/test9.tig" ReadMode
                        contents <- hGetContents handle
                        let absyn = parse $ alexScanTokens contents
                         in print (transExp (ST baseVEnv baseTEnv Outermost initState) absyn) `shouldThrow` anyException
        describe "test10.tig" $ do
                it "error : body of while not unit" $ do
                        handle <- openFile "testcases/test10.tig" ReadMode
                        contents <- hGetContents handle
                        let absyn = parse $ alexScanTokens contents
                         in print (transExp (ST baseVEnv baseTEnv Outermost initState) absyn) `shouldThrow` anyException
        describe "test11.tig" $ do
                it "error hi expr is not int, and index variable erroneously assigned to." $ do
                        handle <- openFile "testcases/test11.tig" ReadMode
                        contents <- hGetContents handle
                        let absyn = parse $ alexScanTokens contents
                         in print (transExp (ST baseVEnv baseTEnv Outermost initState) absyn) `shouldThrow` anyException
        describe "test12.tig" $ do
                it "valid for and let" $ do
                        handle <- openFile "testcases/test12.tig" ReadMode
                        contents <- hGetContents handle
                        let absyn = parse $ alexScanTokens contents
                         in show (transExp (ST baseVEnv baseTEnv Outermost initState) absyn) `shouldBe` show T.UNIT
        describe "test13.tig" $ do
                it "error: comparison of incompatible types" $ do
                        handle <- openFile "testcases/test13.tig" ReadMode
                        contents <- hGetContents handle
                        let absyn = parse $ alexScanTokens contents
                         in print (transExp (ST baseVEnv baseTEnv Outermost initState) absyn) `shouldThrow` anyException
        describe "test14.tig" $ do
                it "error : compare rec with array" $ do
                        handle <- openFile "testcases/test14.tig" ReadMode
                        contents <- hGetContents handle
                        let absyn = parse $ alexScanTokens contents
                         in print (transExp (ST baseVEnv baseTEnv Outermost initState) absyn) `shouldThrow` anyException
        describe "test15.tig" $ do
                it "error : if-then returns non unit" $ do
                        handle <- openFile "testcases/test15.tig" ReadMode
                        contents <- hGetContents handle
                        let absyn = parse $ alexScanTokens contents
                         in print (transExp (ST baseVEnv baseTEnv Outermost initState) absyn) `shouldThrow` anyException
        describe "test16.tig" $ do
                it "error: mutually recursive types thet do not pass through record or array" $ do
                        handle <- openFile "testcases/test16.tig" ReadMode
                        contents <- hGetContents handle
                        let absyn = parse $ alexScanTokens contents
                         in print (transExp (ST baseVEnv baseTEnv Outermost initState) absyn) `shouldThrow` anyException
