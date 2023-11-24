module Chords where

import Data.List
import Data.Maybe

data Chord =
  MkChord
  { chordName :: String
  , chordIntervals :: [Int]
  }
  deriving (Show)

chordCodex :: [Chord]
chordCodex =
  let
    maj, min, dim, aug :: Chord
    maj = MkChord "maj" [0, 4, 7]
    min = MkChord "min" [0, 3, 7]
    dim = MkChord "dim" [0, 3, 6]
    aug = MkChord "aug" [0, 4, 8]

    maj7, min7, dom7, dim7, minMaj7 :: Chord
    maj7    = MkChord "maj7"     [0, 4, 7, 11]
    min7    = MkChord "min7"     [0, 3, 7, 10]
    dom7    = MkChord "7"        [0, 4, 7, 10]
    dim7    = MkChord "dim7"     [0, 3, 6, 9 ]
    minMaj7 = MkChord "min maj7" [0, 3, 7, 11]
  in
    [ maj, min, dim, aug
    , maj7, min7, dom7, dim7, minMaj7
    ]

normalize :: [Int] -> [Int]
normalize ivs =
  case ivs of
    [] -> []
    (root:_) -> map (\ i -> i - root) ivs

invSucc :: [Int] -> [Int]
invSucc ivs =
  case ivs of
    [] -> error "Cannot apply invSucc to empty intervals"
    (root:rest) -> rest ++ [root + 12]

inversions :: Chord -> [([Int], Int)]
inversions chord =
  let
    rootPosition :: ([Int], Int)
    rootPosition = (chordIntervals chord, 0)
  in
    unfoldr (\ (ivs, invNum) ->
               if ivs == fst rootPosition &&
                  invNum > 0
               then Nothing
               else Just ( (ivs, invNum)
                         , (normalize $ invSucc ivs, invNum + 1) )
            ) rootPosition

testAgainstInversions :: [Int] -> [([Int], Int)] -> Maybe ([Int], Int)
testAgainstInversions ivs invs =
  case invs of
    [] -> Nothing
    ((invIvs, invNum):rest) ->
      if normalize ivs == normalize invIvs
      then Just (invIvs, invNum)
      else testAgainstInversions ivs rest

findChordFromIntervals :: [Int] -> Maybe (Chord, Int)
findChordFromIntervals intervals =
  searchThroughCodex chordCodex
  where
    searchThroughCodex :: [Chord] -> Maybe (Chord, Int)
    searchThroughCodex cdx =
      case cdx of
        [] -> Nothing
        (chord:chords) ->
          let
            invs :: [([Int], Int)]
            invs = inversions chord
          in
            case intervals `testAgainstInversions` invs of
              Nothing ->
                searchThroughCodex chords
              Just (matchedIvs, invNum) ->
                Just (chord, invNum)

chordWithInversionToString :: (Chord, Int) -> String
chordWithInversionToString (chord, inv) =
  chordToString chord ++ " in " ++ inversionToString inv
  where
    chordToString = chordName

    inversionToString i
      | i < 0 = error "Chord cannot be in an inversion < 0"
      | i == 0 = "Root position"
      | i == 1 = "1st inversion"
      | i == 2 = "2nd inversion"
      | i == 3 = "3rd inversion"
      | otherwise = show i ++ "th inversion"

