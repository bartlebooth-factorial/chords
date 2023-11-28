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
                 case match of
                   ExactMatch _chord chordInv ->
                     noteToString (notes !! ((-chordInv) `mod` length notes))
                     ++ " " ++ matchToString match
              ) matches
