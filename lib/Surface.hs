module Surface
  ( process
  ) where

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
                 let
                   getRootString ns inv =
                     noteToString (ns !! ((-inv) `mod` length ns))
                 in case match of
                   ExactMatch _chord chordInv ->
                     getRootString notes chordInv
                     ++ " " ++ matchToString match
                   CloseMatch _chord chordInv _transformation ->
                     getRootString notes chordInv
                     ++ " " ++ matchToString match ++ " (close position)"
              ) matches
