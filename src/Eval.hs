module Eval
  ( eval
  ) where

import           Types

eval :: Expr -> Integer
eval (NatNum i) = i
eval (BinOp op expr1 expr2) = (opToFun op) (eval expr1) (eval expr2)
  where
    opToFun op = case op of
      Add   -> (+)
      Minus -> (-)
      Mult  -> (*)
      Div   -> div
      Mod   -> mod
