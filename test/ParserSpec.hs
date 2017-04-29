{-# LANGUAGE ScopedTypeVariables #-}
module ParserSpec (spec) where

import           Parser     (parse)
import           Test.Hspec
import           Types

spec :: Spec
spec = do

  describe "A parser for arithmetic expressions" $ do

    it "should parse [(1,TokenNatNum 1234)]" $
        parse [(1,TokenNatNum 1234)] `shouldBe` Just (NatNum 1234)

    it "should parse the tokens of \"(1+2)\"" $
        parse [ (1,TokenOpeningParenthesis)
              , (2,TokenNatNum 1)
              , (3,TokenAdd)
              , (4,TokenNatNum 2)
              , (5,TokenClosingParenthesis)
              ]
          `shouldBe` Just (BinOp Add (NatNum 1) (NatNum 2))

    it "should parse the tokens of \"(((1*3)-2)%(23/12))\"" $
        parse [ (1,TokenOpeningParenthesis)
              , (2,TokenOpeningParenthesis)
              , (3,TokenOpeningParenthesis)
              , (4,TokenNatNum 1)
              , (5,TokenMult)
              , (6,TokenNatNum 3)
              , (7,TokenClosingParenthesis)
              , (8,TokenMinus)
              , (9,TokenNatNum 2)
              , (10,TokenClosingParenthesis)
              , (11,TokenMod)
              , (12,TokenOpeningParenthesis)
              , (13,TokenNatNum 23)
              , (15,TokenDiv)
              , (16,TokenNatNum 12)
              , (18,TokenClosingParenthesis)
              , (19,TokenClosingParenthesis)
              ]
          `shouldBe`
            Just (BinOp Mod (BinOp Minus (BinOp Mult (NatNum 1) (NatNum 3))
                                         (NatNum 2))
                            (BinOp Div (NatNum 23) (NatNum 12)))
