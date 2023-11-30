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
      let
        intervals = notesToIntervals notes
      in
      case
        allPossibleChords intervals
      of
        [] -> ["Unknown chord"]
        matches ->
          map (\match ->
                 let
                   getRootString :: [Note] -> Int -> String
                   getRootString ns inv =
                     noteToString (ns !! ((-inv) `mod` length ns))
                 in case match of
                   ExactMatch _chord chordInv ->
                     getRootString notes chordInv
                     ++ " " ++ matchToString match
                   CloseMatch _chord chordInv ivTransformation ->
                     getRootString
                       (transformNotes notes (intervals, ivTransformation))
                       chordInv
                     ++ " " ++ matchToString match ++ " (close position)"
              ) matches
