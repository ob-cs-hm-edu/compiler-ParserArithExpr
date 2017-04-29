{-# LANGUAGE ScopedTypeVariables #-}
module ScannerSpec (spec) where

import           Scanner    (scan)
import           Test.Hspec
import           Types

spec :: Spec
spec = do

  describe "A scanner for arithmetic expressions" $ do

    it "should scan \"+\"" $
        scan "+" `shouldBe` Right [(1,TokenAdd)]

    it "should scan \"++++\"" $
        scan "++++" `shouldBe`
                Right [(1,TokenAdd),(2,TokenAdd),(3,TokenAdd),(4,TokenAdd)]

    it "should not scan \"Hello World!\"" $
        scan "Hello World!" `shouldBe` Left (1,'H')

    it "should scan \"-\"" $
        scan "-" `shouldBe` Right [(1,TokenMinus)]

    it "should scan \"-+--\"" $
        scan "-+--" `shouldBe`
              Right [(1,TokenMinus),(2,TokenAdd),(3,TokenMinus),(4,TokenMinus)]

    it "should scan \"12345\"" $
        scan "12345" `shouldBe` Right [(1,TokenNatNum 12345)]

    it "should scan \"123123*(3123/(123-(123+123)))\"" $
        scan "123123*(3123/(123-(123+123)))" `shouldBe`
          Right [ (1,TokenNatNum 123123)
                , (7,TokenMult)
                , (8,TokenOpeningParenthesis)
                , (9,TokenNatNum 3123)
                , (13,TokenDiv)
                , (14,TokenOpeningParenthesis)
                , (15,TokenNatNum 123)
                , (18,TokenMinus)
                , (19,TokenOpeningParenthesis)
                , (20,TokenNatNum 123)
                , (23,TokenAdd)
                , (24,TokenNatNum 123)
                , (27,TokenClosingParenthesis)
                , (28,TokenClosingParenthesis)
                , (29,TokenClosingParenthesis)
                ]
