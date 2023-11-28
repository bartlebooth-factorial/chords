module Main where

import System.Environment (getArgs)

import Chords
import Parser
import Translation

process :: String -> [String]
process s =
  case parseNotes s of
    Left err -> [show err]
    Right notes ->
      case
        allPossibleChords $ notesToIntervals notes
      of
        [] -> ["Unknown chord"]
        matches ->
          map (\match ->
                 case match of
                   ExactMatch _chord chordInv ->
                     noteToString (notes !! ((-chordInv) `mod` length notes))
                     ++ " " ++ matchToString match
              ) matches

main :: IO Int
main =
  do { args <- getArgs
     ; case args of
         [] -> do { putStrLn "Error: 1 argument needed, none given"
                  ; return 1 }
         str:_ -> do { mapM_ putStrLn (process str)
                     ; return 0 } }
