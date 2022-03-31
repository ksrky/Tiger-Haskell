module LexerSpec where

import Lexer
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
        describe "all tokens" $ do
                it "while" $
                        alexScanTokens "while" `shouldBe` [TkWhile (AlexPn 0 1 1)]
                it "for" $
                        alexScanTokens "for" `shouldBe` [TkFor (AlexPn 0 1 1)]
                it "to" $
                        alexScanTokens "to" `shouldBe` [TkTo (AlexPn 0 1 1)]
                it "break" $
                        alexScanTokens "break" `shouldBe` [TkBreak (AlexPn 0 1 1)]
                it "let" $
                        alexScanTokens "let" `shouldBe` [TkLet (AlexPn 0 1 1)]
                it "in" $
                        alexScanTokens "in" `shouldBe` [TkIn (AlexPn 0 1 1)]
                it "end" $
                        alexScanTokens "end" `shouldBe` [TkEnd (AlexPn 0 1 1)]
                it "function" $
                        alexScanTokens "function" `shouldBe` [TkFunction (AlexPn 0 1 1)]
                it "var" $
                        alexScanTokens "var" `shouldBe` [TkVar (AlexPn 0 1 1)]
                it "type" $
                        alexScanTokens "type" `shouldBe` [TkType (AlexPn 0 1 1)]
                it "array" $
                        alexScanTokens "array" `shouldBe` [TkArray (AlexPn 0 1 1)]
                it "if" $
                        alexScanTokens "if" `shouldBe` [TkIf (AlexPn 0 1 1)]
                it "then" $
                        alexScanTokens "then" `shouldBe` [TkThen (AlexPn 0 1 1)]
                it "else" $
                        alexScanTokens "else" `shouldBe` [TkElse (AlexPn 0 1 1)]
                it "do" $
                        alexScanTokens "do" `shouldBe` [TkDo (AlexPn 0 1 1)]
                it "of" $
                        alexScanTokens "of" `shouldBe` [TkOf (AlexPn 0 1 1)]
                it "nil" $
                        alexScanTokens "nil" `shouldBe` [TkNil (AlexPn 0 1 1)]
                it "," $
                        alexScanTokens "," `shouldBe` [TkComma (AlexPn 0 1 1)]
                it ":" $
                        alexScanTokens ":" `shouldBe` [TkColon (AlexPn 0 1 1)]
                it ";" $
                        alexScanTokens ";" `shouldBe` [TkSemicolon (AlexPn 0 1 1)]
                it "(" $
                        alexScanTokens "(" `shouldBe` [TkLParen (AlexPn 0 1 1)]
                it ")" $
                        alexScanTokens ")" `shouldBe` [TkRParen (AlexPn 0 1 1)]
                it "[" $
                        alexScanTokens "[" `shouldBe` [TkLBrack (AlexPn 0 1 1)]
                it "]" $
                        alexScanTokens "]" `shouldBe` [TkRBrack (AlexPn 0 1 1)]
                it "{" $
                        alexScanTokens "{" `shouldBe` [TkLBrace (AlexPn 0 1 1)]
                it "}" $
                        alexScanTokens "}" `shouldBe` [TkRBrace (AlexPn 0 1 1)]
                it "." $
                        alexScanTokens "." `shouldBe` [TkDot (AlexPn 0 1 1)]
                it "+" $
                        alexScanTokens "+" `shouldBe` [TkPlus (AlexPn 0 1 1)]
                it "-" $
                        alexScanTokens "-" `shouldBe` [TkMinus (AlexPn 0 1 1)]
                it "*" $
                        alexScanTokens "*" `shouldBe` [TkTimes (AlexPn 0 1 1)]
                it "/" $
                        alexScanTokens "/" `shouldBe` [TkDevide (AlexPn 0 1 1)]
                it "=" $
                        alexScanTokens "=" `shouldBe` [TkEQ (AlexPn 0 1 1)]
                it "<>" $
                        alexScanTokens "<>" `shouldBe` [TkNEQ (AlexPn 0 1 1)]
                it "<" $
                        alexScanTokens "<" `shouldBe` [TkLT (AlexPn 0 1 1)]
                it "<=" $
                        alexScanTokens "<=" `shouldBe` [TkLE (AlexPn 0 1 1)]
                it ">" $
                        alexScanTokens ">" `shouldBe` [TkGT (AlexPn 0 1 1)]
                it ">=" $
                        alexScanTokens ">=" `shouldBe` [TkGE (AlexPn 0 1 1)]
                it "&" $
                        alexScanTokens "&" `shouldBe` [TkAnd (AlexPn 0 1 1)]
                it "|" $
                        alexScanTokens "|" `shouldBe` [TkOr (AlexPn 0 1 1)]
                it ":=" $
                        alexScanTokens ":=" `shouldBe` [TkAssign (AlexPn 0 1 1)]
                it "<ident>" $
                        alexScanTokens "ident" `shouldBe` [TkId ("ident", AlexPn 0 1 1)]
                it "<int>" $
                        alexScanTokens "3" `shouldBe` [TkInt (3, AlexPn 0 1 1)]
                it "<string>" $
                        alexScanTokens "\"hello\"" `shouldBe` [TkString ("hello", AlexPn 0 1 1)]