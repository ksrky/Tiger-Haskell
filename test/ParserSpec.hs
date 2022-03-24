module ParserSpec where

import Absyn
import Lexer (alexScanTokens)
import Parser (parse)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
        describe "" $
                it "1+2" $
                        parse (alexScanTokens "1+2") `shouldBe` OpExp{leftExp = IntExp 1, operExp = PlusOp, rightExp = IntExp 2, posExp = (1, 2)}