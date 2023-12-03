module Surface
  ( process
  ) where

import Text.Parsec (ParseError)

import Chords
import Parser
import Translation

type Result = ([Note], [Int], [Match])

getResult :: String -> Either ParseError Result
getResult s =
  case parseNotes s of
    Left err -> Left err
    Right notes ->
      let intervals = notesToIntervals notes
      in Right ( notes
               , intervals
               , allPossibleChords intervals)

matchToStringContextual :: ([Note], [Int], Match) -> Bool -> String
matchToStringContextual (notes, intervals, match) verbose =
  case match of
    ExactMatch _chord chordInv ->
      (if verbose
       then notesToString notes ++ " ->\n  "
       else "") ++
      getRootString notes chordInv ++ " " ++
      matchToString match
    CloseMatch _chord chordInv ivTransformation ->
      let cpNotes = transformNotes notes (intervals, ivTransformation)
      in (if verbose
          then notesToString notes ++ " -> " ++
               notesToString cpNotes ++ " (close position) ->\n  "
          else "") ++
         getRootString cpNotes chordInv ++ " " ++
         matchToString match
  where
    getRootString :: [Note] -> Int -> String
    getRootString ns inv =
      noteToString (ns !! ((-inv) `mod` length ns))

process :: String -> Bool -> [String]
process s verboseFlag =
  case getResult s of
    Left err -> [show err]
    Right (notes, intervals, matches) ->
      case matches of
        [] -> ["Unknown chord"]
        _ -> map (\ match ->
                    matchToStringContextual
                      (notes, intervals, match)
                      verboseFlag
                 ) matches
