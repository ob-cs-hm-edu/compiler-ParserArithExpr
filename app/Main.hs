module Main where

import           Control.Monad (when)
import           Eval          (eval)
import           Parser        (parse)
import           Scanner       (scan)
import           System.IO     (BufferMode (NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn $ "Geben Sie am Prompt einen Ausdruck zum Parsen\n"
              ++ "oder \":q\" zum Beenden ein."
    main'

main' :: IO ()
main' = do
    putStr "> "
    input <- getLine
    when (input /= ":q") $ do
      -- Scanner
      case scan input of
        Left (p,c) ->
          putStrLn $ ">>> scan error at position "
                   ++ show p ++ ", char "++ show c
        Right tokens -> do
          putStrLn $ replicate 80 '-' ++ "\n"
                   ++ "Tokens = " ++ show tokens
          -- Parser
          let parseResult = parse tokens
          case parseResult of
            Nothing ->
              putStrLn ">>> parse error"
            Just expr -> do
              putStrLn $ replicate 80 '-' ++ "\n"
                       ++ "Expr = " ++ show parseResult
              -- Evaluator
              putStrLn $ replicate 80 '-' ++ "\n"
                       ++ "Evaluated = " ++ show (eval expr)
      putStrLn $ replicate 80 '-'
      main'
