{-# LANGUAGE ScopedTypeVariables #-}
module EvalSpec (spec) where

import           Eval       (eval)
import           Test.Hspec
import           Types

spec :: Spec
spec = do

  describe "An evaluator for arithmetic expressions" $ do

    it "should eval NatNum 1234" $
        eval (NatNum 1234) `shouldBe` 1234

    it "should eval (BinOp Add (NatNum 23) (NatNum 2))" $
        eval (BinOp Add (NatNum 23) (NatNum 2)) `shouldBe` 25

    it "should eval the expr for (((3646%244)+(234-1))*12)" $
        eval (BinOp Mult (BinOp Add (BinOp Mod (NatNum 3646) (NatNum 244))
                                    (BinOp Minus (NatNum 234) (NatNum 1)))
                         (NatNum 12))
          `shouldBe` 5556
