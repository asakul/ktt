{-# LANGUAGE OverloadedStrings #-}

import Productivity.TimeTracking.KTT

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T
import           Time.Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests" [ testParsing ]

testParsing :: TestTree
testParsing = testGroup "Parsing" [ testFrameParsing ]

testFrameParsing :: TestTree
testFrameParsing = testGroup "Frame parsing" [ testWFEStartParsing,
  testWFEStartParsingTwoTags,
  testWFEStartParsingUnicode,
  testWFEStopParsing
  ]

testWFEStartParsing = testCase "WFEStart parsing" $ do
  case parseWorkFlowEntry "2017/11/24 10:27:00 START Foobar +foo" of
    Left err -> assertFailure $ "Parser returned failure: " ++ T.unpack err
    Right entry -> assertEqual "Invalid FrameStart entry" (FrameStart "Foobar" ["foo"] (DateTime (Date 2017 November 24) (TimeOfDay 10 27 0 0))) entry

testWFEStartParsingTwoTags = testCase "WFEStart parsing: two tags" $ do
  case parseWorkFlowEntry "2017/11/24 10:27:00 START Foobar +foo +bar" of
    Left err -> assertFailure $ "Parser returned failure: " ++ T.unpack err
    Right entry -> assertEqual "Invalid FrameStart entry" (FrameStart "Foobar" ["foo", "bar"] (DateTime (Date 2017 November 24) (TimeOfDay 10 27 0 0))) entry

testWFEStartParsingUnicode = testCase "WFEStart parsing: simple unicode" $ do
  case parseWorkFlowEntry "2017/11/24 10:27:00 START ∀ ε > 0 ∃ δ +foo" of
    Left err -> assertFailure $ "Parser returned failure: " ++ T.unpack err
    Right entry -> assertEqual "Invalid FrameStart entry" (FrameStart "∀ ε > 0 ∃ δ" ["foo"] (DateTime (Date 2017 November 24) (TimeOfDay 10 27 0 0))) entry

testWFEStopParsing = testCase "WFEStop parsing" $ do
  case parseWorkFlowEntry "2017/11/24 10:27:00 STOP Foobar" of
    Left err -> assertFailure $ "Parser returned failure: " ++ T.unpack err
    Right entry -> assertEqual "Invalid FrameStop entry" (FrameStop "Foobar" (DateTime (Date 2017 November 24) (TimeOfDay 10 27 0 0))) entry

