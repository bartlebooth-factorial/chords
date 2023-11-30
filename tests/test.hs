module Main where

import Data.Either

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Chords
import Parser
import Surface

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [parserTests, matchingTests]

parserTests :: TestTree
parserTests = testGroup "Parser"
  [ testCase "empty input" $
      assertBool "empty input should fail"
      (isLeft (parseNotes ""))

  , testCase "single note, natural" $
      parseNotes "C" @?=
      Right [('C', 0)]

  , testCase "single note, flat" $
      parseNotes "Cb" @?=
      Right [('C', -1)]

  , testCase "single note, sharp" $
      parseNotes "C#" @?=
      Right [('C', 1)]

  , testCase "multiple notes, all natural" $
      parseNotes "CDE" @?=
      Right [('C', 0), ('D', 0), ('E', 0)]

  , testCase "whitespace shouldn't matter" $
      assertEqual ""
      (parseNotes "ABCDEFG") (parseNotes " A B C D   E F G ")
  ]

matchingTests :: TestTree
matchingTests = testGroup "Matching" [exactTests, closeTests]

processNoV :: String -> [String]
processNoV = (flip process) False

exactTests :: TestTree
exactTests = testGroup "Exact Matching"
  [ testCase "C major triad" $
    processNoV "C E G" @?=
    ["C maj in Root position"]

  , testCase "Ab major 7, 3rd inversion" $
    processNoV "G Ab C Eb" @?=
    ["Ab maj7 in 3rd inversion"]
  ]

closeTests :: TestTree
closeTests = testGroup "Close Matching"
  [ testCase "C major spread triad" $
    processNoV "C G E" @?=
    ["C maj in Root position (close position)"]

  , testCase "B 7, root doubled in the bass" $
    processNoV "B B D# F# A" @?=
    ["B 7 in Root position (close position)"]

  , testCase "C sus4, F sus2 in 2nd inv, C doubled" $
    processNoV "C G C F" @?=
    ["C sus4 in Root position (close position)",
     "F sus2 in 2nd inversion (close position)"]
  ]

propertyTests :: TestTree
propertyTests = testGroup "Property tests" [parserPropTests, matchingPropTests]

parserPropTests :: TestTree
parserPropTests = testGroup "Parser property tests"
  [ testProperty "whitespace on input ends shouldn't affect parser accept or reject" $
    \s -> isLeft (parseNotes (s :: String)) ===
          isLeft (parseNotes (" " ++ s ++ " "))
  ]

matchingPropTests :: TestTree
matchingPropTests = testGroup "Matching-strategies property tests"
  [ testProperty "closePosition should be idempotent" $
    \pivs ->
      let
        ivs = map getPositive (pivs :: [Positive Int])
      in
        closePosition ivs === closePosition (closePosition ivs)
  , testProperty
    ("insertion of a duplicate pitch into intervals results in "
     ++ "only close-position matches if any") $
    \pivs pindex ->
      let
        ivs = map getPositive (pivs :: [Positive Int])
        index = getPositive (pindex :: Positive Int)

        insertDuplicate :: [a] -> Int -> [a]
        insertDuplicate xs i =
          (take i xs) ++ [(xs !! i)] ++ (drop i xs)
      in
        index < (length ivs) ==>
        (all (\ m ->
               case m of
                 CloseMatch _ _ _
                   -> True
                 _ -> False
              ) (allPossibleChords (insertDuplicate ivs index))
         === True)
  ]

