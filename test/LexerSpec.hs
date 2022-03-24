module LexerSpec where

import Lexer
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
        describe "test1.tig" $
                it "let" $
                        alexScanTokens "let" `shouldBe` [TkLet (AlexPn 0 1 1)]
        describe "test1.tig" $
                it "type" $
                        alexScanTokens "type" `shouldBe` [TkType (AlexPn 0 1 1)]
        describe "test1.tig" $
                it "=" $
                        alexScanTokens "=" `shouldBe` [TkEQ (AlexPn 0 1 1)]
        describe "test1.tig" $
                it "arrtype" $
                        alexScanTokens "arrtype" `shouldBe` [TkId ("arrtype", AlexPn 0 1 1)]
        describe "test1.tig" $
                it "int" $
                        alexScanTokens "int" `shouldBe` [TkId ("int", AlexPn 0 1 1)]
        describe "" $
                it ":=" $
                        alexScanTokens ":=" `shouldBe` [TkAssign (AlexPn 0 1 1)]
        describe "" $
                it "<>" $
                        alexScanTokens "<>" `shouldBe` [TkNEQ (AlexPn 0 1 1)]