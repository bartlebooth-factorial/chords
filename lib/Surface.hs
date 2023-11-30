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

matchToStringContextual :: ([Note], [Int], Match) -> String
matchToStringContextual (notes, intervals, match) =
  case match of
    ExactMatch _chord chordInv ->
      getRootString notes chordInv
      ++ " " ++ matchToString match
    CloseMatch _chord chordInv ivTransformation ->
      getRootString
        (transformNotes notes (intervals, ivTransformation))
        chordInv
      ++ " " ++ matchToString match ++ " (close position)"
  where
    getRootString :: [Note] -> Int -> String
    getRootString ns inv =
      noteToString (ns !! ((-inv) `mod` length ns))

process :: String -> [String]
process s =
  case getResult s of
    Left err -> [show err]
    Right (notes, intervals, matches) ->
      case matches of
        [] -> ["Unknown chord"]
        _ -> map (\ match ->
                    matchToStringContextual
                      (notes, intervals, match)
                 ) matches
