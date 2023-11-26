module Chords where

import Data.List (unfoldr, sortBy)

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

    sus4, sus2 :: Chord
    sus4 = MkChord "sus4" [0, 5, 7]
    sus2 = MkChord "sus2" [0, 2, 7]

    maj6 :: Chord
    maj6 = MkChord "maj6" [0, 4, 7, 9]
  in
    [ maj, min, dim, aug
    , maj7, min7, dom7, dim7, minMaj7
    , sus4, sus2
    , maj6
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

checkChord :: [Int] -> Chord -> Maybe (Chord, Int)
checkChord intervals chord =
  let invs = inversions chord in
    case intervals `testAgainstInversions` invs of
      Nothing -> Nothing
      Just (matchedIntervals, invNum) ->
        Just (chord, invNum)

allPossibleChords :: [Int] -> [(Chord, Int)]
allPossibleChords intervals =
  let results = map (checkChord intervals) chordCodex in
    occamSort $ gather results
  where
    gather :: [Maybe a] -> [a]
    gather [] = []
    gather (x:xs) =
      case x of
        Just a -> a : gather xs
        Nothing -> gather xs

    occamSort :: [(a, Int)] -> [(a, Int)]
    occamSort = sortBy (\ (_, invNum1) (_, invNum2) ->
                          compare invNum1 invNum2)

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

