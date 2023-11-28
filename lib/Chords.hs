module Chords
  ( Chord (..)
  , Match (..)
  , allPossibleChords
  ) where

import Data.List (unfoldr, sort, nub)

data Chord =
  MkChord
  { chordName :: String
  , chordIntervals :: [Int]
  }
  deriving (Show, Eq)

data Match =
  ExactMatch
  { matchedChord :: Chord
  , matchedInvNum :: Int
  }
  | CloseMatch
  { matchedChord :: Chord
  , matchedInvNum :: Int
  , transformation :: [Int]
  }
  deriving (Show, Eq)

instance Ord Match where
  (<=) (ExactMatch _ invNumX) (ExactMatch _ invNumY)
    = invNumX <= invNumY

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

closePosition :: [Int] -> [Int]
closePosition ivs =
  case ivs of
    [] -> []
    [root] -> [root]
    (root : rest) ->
      if root == 0
      then root :
           (sort . nub . (map octaveReduce)) rest
      else error "Root cannot be non-zero"
      where
        octaveReduce :: Int -> Int
        octaveReduce i
          | i < 0 = error "Interval cannot be < 0"
          | i == 0 = 12
          | i <= 12 = i
          | i > 12 = octaveReduce (i - 12)

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

checkChord :: [Int] -> Chord -> Maybe Match
checkChord intervals chord =
  let invs = inversions chord in
    case intervals `testAgainstInversions` invs of
      Nothing ->
        let closeIvs = closePosition intervals in
        case closeIvs `testAgainstInversions` invs of
          Nothing -> Nothing
          Just (_matchedIntervals, invNum) ->
            Just (CloseMatch chord invNum closeIvs)
      Just (_matchedIntervals, invNum) ->
        Just (ExactMatch chord invNum)

allPossibleChords :: [Int] -> [Match]
allPossibleChords intervals =
  let results = map (checkChord intervals) chordCodex in
    sort $ gather results
  where
    gather :: [Maybe a] -> [a]
    gather [] = []
    gather (x:xs) =
      case x of
        Just a -> a : gather xs
        Nothing -> gather xs

