{-# LANGUAGE OverloadedStrings #-}

import Productivity.TimeTracking.KTT

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T
import           Time.Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit tests" [ testParsing, testCombining ]

testParsing :: TestTree
testParsing = testGroup "Parsing" [ testFrameParsing ]

testFrameParsing :: TestTree
testFrameParsing = testGroup "Frame parsing" [ testWFEStartParsing,
  testWFEStartParsingWithLineend,
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

testWFEStartParsingWithLineend = testCase "WFEStart parsing with line end" $ do
  case parseWorkFlowEntry "2017/11/24 10:27:00 START Foobar   \r" of
    Left err -> assertFailure $ "Parser returned failure: " ++ T.unpack err
    Right entry -> assertEqual "Invalid FrameStart entry" (FrameStart "Foobar" [] (DateTime (Date 2017 November 24) (TimeOfDay 10 27 0 0))) entry

testCombining :: TestTree
testCombining = testGroup "Frame combining" [ testCombiningSimple ]

testCombiningSimple = testCase "Combining: simple" $ do
  let (frames, _) = combineFrames (WorkFlow [
        (FrameStart "Foobar" ["foo", "bar"] (DateTime (Date 2017 November 24) (TimeOfDay 10 27 0 0))),
        (FrameStop "Foobar" (DateTime (Date 2017 November 24) (TimeOfDay 11 27 0 0))) ])

  length frames @?= 1
  (Frame "Foobar" ["foo", "bar"] (Just (DateTime (Date 2017 November 24) (TimeOfDay 10 27 0 0))) (Just (DateTime (Date 2017 November 24) (TimeOfDay 11 27 0 0)))) @?= (head frames)
