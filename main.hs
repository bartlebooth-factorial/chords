module Main where

import System.IO
import System.Environment

import Chords
import Parser
import Translation

test :: String -> String
test s =
  case parseNotes s of
    Left err -> show err
    Right notes ->
      case
        findChordFromIntervals $ notesToIntervals notes
      of
        Nothing -> "Unknown chord"
        Just chord@(chordName, chordInv) ->
          fst (notes !! ((-chordInv) `mod` length notes)) : " " ++
          chordWithInversionToString chord

main :: IO Int
main =
  do { args <- getArgs
     ; case args of
         [] -> do { putStrLn "Error: 1 argument needed, none given"
                  ; return 1 }
         str:_ -> do { putStrLn (test str)
                     ; return 0 } }
