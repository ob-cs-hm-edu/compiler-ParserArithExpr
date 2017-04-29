module Scanner
  ( scan
  ) where

import           Control.Applicative ((<$>))
import           Data.Char           (isDigit)
import           Data.List           (span)
import           Types               (Position, Token (..), TokenWithPosition)

scan :: String -> Either (Position, Char) [TokenWithPosition]
scan = scan' . zip [1..]

scan' :: [(Position, Char)] -> Either (Position, Char) [TokenWithPosition]
scan' []             = Right []
scan' ((p,'('):rest) = ((p,TokenOpeningParenthesis ):) <$> scan' rest
scan' ((p,')'):rest) = ((p,TokenClosingParenthesis ):) <$> scan' rest
scan' ((p,'+'):rest) = ((p,TokenAdd                ):) <$> scan' rest
scan' ((p,'-'):rest) = ((p,TokenMinus              ):) <$> scan' rest
scan' ((p,'*'):rest) = ((p,TokenMult               ):) <$> scan' rest
scan' ((p,'/'):rest) = ((p,TokenDiv                ):) <$> scan' rest
scan' ((p,'%'):rest) = ((p,TokenMod                ):) <$> scan' rest
scan' posStr =
  let (posIntStr,rest) = span (isDigit . snd) posStr
  in if not $ null posIntStr
     then (( fst $ head posIntStr
           , TokenNatNum (read $ map snd posIntStr)):) <$> scan' rest
     else Left $ head posStr
