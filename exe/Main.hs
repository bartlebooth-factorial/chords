module Main where

import System.Environment (getArgs)

import Surface

main :: IO Int
main =
  let
    usageMessage = "Usage: chords [-v] \"[NOTES]\""
  in
    do { args <- getArgs
       ; case args of
           [] -> do { putStrLn usageMessage
                    ; return 1 }
           str:[] -> do { mapM_ putStrLn (process str False)
                        ; return 0 }
           flag:str:[] -> do { let verbose = (flag == "-v")
                               in mapM_ putStrLn (process str verbose)
                             ;    return 0 }
           _ -> do { putStrLn usageMessage
                   ; return 1 } }
