module Translation
  ( noteToString
  , notesToIntervals
  , matchToString
  ) where

import Parser (Note)
import Chords (Chord (..), Match (..))

noteToString :: Note -> String
noteToString (noteName, delta) =
  noteName : accidentals
  where
    accidentals :: String
    accidentals
      | delta < 0 = replicate (-delta) 'b'
      | delta == 0 = ""
      | delta > 0 = replicate delta '#'

noteNameLadder :: [(Char, Int)]
noteNameLadder =
  cycle
  [('A', 2), ('B', 1), ('C', 2), ('D', 2), ('E', 1), ('F', 2), ('G', 2)]

noteNameDistance :: Char -> Char -> Int
noteNameDistance nx ny =
  let
    noteLadder =
      dropWhile (\ (noteName, _)
                  -> noteName /= nx
                ) noteNameLadder

    climb ladder =
      case ladder of
        ((noteName, distToNext) : rest) ->
          if noteName == ny
          then 0
          else distToNext + climb rest

    dist = climb noteLadder
  in
    if dist == 0
    then 12
    else dist

octaveReduce :: Int -> Int
octaveReduce i
  | i < 0 = error "Interval cannot be < 0"
  | i == 0 = 12
  | i <= 12 = i
  | i > 12 = octaveReduce (i - 12)

notesToIntervals :: [Note] -> [Int]
notesToIntervals notes =
  case notes of
    [] -> []
    (noteName, delta) : rest ->
      0 :
      makeIntervals noteName (-delta) rest
      where
        makeIntervals :: Char -> Int -> [Note] -> [Int]
        makeIntervals referenceName offset noteList =
          case noteList of
            [] -> []
            ((nName, nDelta) : ns) ->
              let
                dist = noteNameDistance referenceName nName
                iv = octaveReduce $ dist + nDelta + offset
              in
                if iv < 0
                then error "Notes must be given in low-to-high order"
                else
                  iv :
                  makeIntervals nName (iv - nDelta) ns

matchToString :: Match -> String
matchToString (ExactMatch chord invNum) =
  chordName chord ++ " in " ++ inversionToString invNum
  where
    inversionToString i
      | i < 0 = error "Chord cannot be in an inversion < 0"
      | i == 0 = "Root position"
      | i == 1 = "1st inversion"
      | i == 2 = "2nd inversion"
      | i == 3 = "3rd inversion"
      | otherwise = show i ++ "th inversion"
