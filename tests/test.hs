module Main where

import Data.Either

import Test.Tasty
import Test.Tasty.HUnit

import Parser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests" [parserTests]

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

  , testCase "Whitespace shouldn't matter" $
      assertEqual ""
      (parseNotes "ABCDEFG") (parseNotes " A B C D   E F G ")
  ]

