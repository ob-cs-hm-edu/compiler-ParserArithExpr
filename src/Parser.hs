module Parser
  ( parse
  ) where

import           Types

parse :: [TokenWithPosition] -> Maybe Expr
parse tokens =
  case parseExpr tokens of
    [(expr, [])] -> Just expr
    _            -> Nothing

parseExpr :: [TokenWithPosition] -> [(Expr, [TokenWithPosition])]
parseExpr ((_, TokenNatNum num) : rest) = [(NatNum num, rest)]
parseExpr ((_, TokenOpeningParenthesis) : rest) =
  case parseExpr rest of
    [(expr, rest')] -> case rest' of
      (_,opToken) : rest''
          | opToken `elem`
            [TokenAdd, TokenMinus, TokenMult, TokenDiv, TokenMod] ->
        case parseExpr rest'' of
          [(expr', (_,TokenClosingParenthesis) : rest''')]
            -> [(BinOp (opTokenToOp opToken) expr expr', rest''')]
          _ -> []
      _ -> []
    _ -> []
  where
    opTokenToOp :: Token -> Op
    opTokenToOp token = case token of
      TokenAdd   -> Add
      TokenMinus -> Minus
      TokenMult  -> Mult
      TokenDiv   -> Div
      TokenMod   -> Mod
parseExpr _ = []
