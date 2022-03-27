module LexerSpec where

import Lexer
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
        describe "" $
                it "while" $
                        alexScanTokens "while" `shouldBe` [TkWhile (AlexPn 0 1 1)]
        describe "" $
                it "for" $
                        alexScanTokens "for" `shouldBe` [TkFor (AlexPn 0 1 1)]
        describe "" $
                it "to" $
                        alexScanTokens "to" `shouldBe` [TkTo (AlexPn 0 1 1)]
        describe "" $
                it "break" $
                        alexScanTokens "break" `shouldBe` [TkBreak (AlexPn 0 1 1)]
        describe "" $
                it "let" $
                        alexScanTokens "let" `shouldBe` [TkLet (AlexPn 0 1 1)]
        describe "" $
                it "in" $
                        alexScanTokens "in" `shouldBe` [TkIn (AlexPn 0 1 1)]
        describe "" $
                it "end" $
                        alexScanTokens "end" `shouldBe` [TkEnd (AlexPn 0 1 1)]

        describe "" $
                it "function" $
                        alexScanTokens "function" `shouldBe` [TkFunction (AlexPn 0 1 1)]
        describe "" $
                it "var" $
                        alexScanTokens "var" `shouldBe` [TkVar (AlexPn 0 1 1)]
        describe "" $
                it "type" $
                        alexScanTokens "type" `shouldBe` [TkType (AlexPn 0 1 1)]
        describe "" $
                it "array" $
                        alexScanTokens "array" `shouldBe` [TkArray (AlexPn 0 1 1)]
        describe "" $
                it "if" $
                        alexScanTokens "if" `shouldBe` [TkIf (AlexPn 0 1 1)]
        describe "" $
                it "then" $
                        alexScanTokens "then" `shouldBe` [TkThen (AlexPn 0 1 1)]
        describe "" $
                it "else" $
                        alexScanTokens "else" `shouldBe` [TkElse (AlexPn 0 1 1)]
        describe "" $
                it "do" $
                        alexScanTokens "do" `shouldBe` [TkDo (AlexPn 0 1 1)]
        describe "" $
                it "of" $
                        alexScanTokens "of" `shouldBe` [TkOf (AlexPn 0 1 1)]
        describe "" $
                it "nil" $
                        alexScanTokens "nil" `shouldBe` [TkNil (AlexPn 0 1 1)]
        describe "" $
                it "," $
                        alexScanTokens "," `shouldBe` [TkComma (AlexPn 0 1 1)]
        describe "" $
                it ":" $
                        alexScanTokens ":" `shouldBe` [TkColon (AlexPn 0 1 1)]
        describe "" $
                it ";" $
                        alexScanTokens ";" `shouldBe` [TkSemicolon (AlexPn 0 1 1)]
        describe "" $
                it "(" $
                        alexScanTokens "(" `shouldBe` [TkLParen (AlexPn 0 1 1)]
        describe "" $
                it ")" $
                        alexScanTokens ")" `shouldBe` [TkRParen (AlexPn 0 1 1)]
        describe "" $
                it "[" $
                        alexScanTokens "[" `shouldBe` [TkLBrack (AlexPn 0 1 1)]
        describe "" $
                it "]" $
                        alexScanTokens "]" `shouldBe` [TkRBrack (AlexPn 0 1 1)]
        describe "" $
                it "{" $
                        alexScanTokens "{" `shouldBe` [TkLBrace (AlexPn 0 1 1)]
        describe "" $
                it "}" $
                        alexScanTokens "}" `shouldBe` [TkRBrace (AlexPn 0 1 1)]
        describe "" $
                it "." $
                        alexScanTokens "." `shouldBe` [TkDot (AlexPn 0 1 1)]
        describe "" $
                it "+" $
                        alexScanTokens "+" `shouldBe` [TkPlus (AlexPn 0 1 1)]
        describe "" $
                it "-" $
                        alexScanTokens "-" `shouldBe` [TkMinus (AlexPn 0 1 1)]
        describe "" $
                it "*" $
                        alexScanTokens "*" `shouldBe` [TkTimes (AlexPn 0 1 1)]
        describe "" $
                it "/" $
                        alexScanTokens "/" `shouldBe` [TkDevide (AlexPn 0 1 1)]
        describe "" $
                it "=" $
                        alexScanTokens "=" `shouldBe` [TkEQ (AlexPn 0 1 1)]
        describe "" $
                it "<>" $
                        alexScanTokens "<>" `shouldBe` [TkNEQ (AlexPn 0 1 1)]
        describe "" $
                it "<" $
                        alexScanTokens "<" `shouldBe` [TkLT (AlexPn 0 1 1)]
        describe "" $
                it "<=" $
                        alexScanTokens "<=" `shouldBe` [TkLE (AlexPn 0 1 1)]
        describe "" $
                it ">" $
                        alexScanTokens ">" `shouldBe` [TkGT (AlexPn 0 1 1)]
        describe "" $
                it ">=" $
                        alexScanTokens ">=" `shouldBe` [TkGE (AlexPn 0 1 1)]
        describe "" $
                it "&" $
                        alexScanTokens "&" `shouldBe` [TkAnd (AlexPn 0 1 1)]
        describe "" $
                it "|" $
                        alexScanTokens "|" `shouldBe` [TkOr (AlexPn 0 1 1)]
        describe "" $
                it ":=" $
                        alexScanTokens ":=" `shouldBe` [TkAssign (AlexPn 0 1 1)]
        describe "" $
                it "ident" $
                        alexScanTokens "ident" `shouldBe` [TkId ("ident", AlexPn 0 1 1)]
        describe "" $
                it "int" $
                        alexScanTokens "3" `shouldBe` [TkInt (3, AlexPn 0 1 1)]
        describe "string" $
                it "\"hello\"" $
                        alexScanTokens "\"hello\"" `shouldBe` [TkString ("hello", AlexPn 0 1 1)]