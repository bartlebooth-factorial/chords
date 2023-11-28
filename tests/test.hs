module Main where

import Data.Either

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Chords
import Parser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests" [parserTests]

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
  ]

