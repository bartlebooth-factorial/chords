module Main where

import System.Environment (getArgs)

import Surface

main :: IO Int
main =
  do { args <- getArgs
     ; case args of
         [] -> do { putStrLn "Error: 1 argument needed, none given"
                  ; return 1 }
         str:_ -> do { mapM_ putStrLn (process str)
                     ; return 0 } }
