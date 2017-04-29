module Types
  ( Token(..)
  , Position
  , TokenWithPosition
  , Expr(..)
  , Op(..)
  ) where

type Position = Int
type TokenWithPosition = (Position, Token)

data Token =
      TokenNatNum Integer     -- ^ represents Integer
    | TokenOpeningParenthesis -- ^ '('
    | TokenClosingParenthesis -- ^ ')'
    | TokenAdd                -- ^ '+'
    | TokenMinus              -- ^ '-'
    | TokenMult               -- ^ '*'
    | TokenDiv                -- ^ '/'
    | TokenMod                -- ^ '%'
  deriving (Show, Eq)

data Expr = NatNum Integer      -- Integer
          | BinOp Op Expr Expr  -- ( expr op expr )
  deriving (Show, Eq)

data Op = Add | Minus | Mult | Div | Mod
  deriving (Show, Eq)
