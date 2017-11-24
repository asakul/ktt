{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid
import Data.Maybe
import Data.Hourglass
import Data.Text.Prettyprint.Doc (Doc, pretty, (<+>), vsep, hsep, brackets)
import Console.Options
import Text.Megaparsec
import qualified Data.Text as T
import Time.System

import System.Directory
import System.Environment
import Safe

import Productivity.TimeTracking.KTT

getJournalFilename = do
  e <- lookupEnv "KTT_JOURNAL_FILE"
  case e of
    Just fname -> return fname
    _ -> getXdgDirectory XdgData "ktt.journal"

main :: IO ()
main = do
  journalFile <- getJournalFilename
  defaultMain $ do
    programName "tt"
    programDescription "K time tracker"
    command "start" $ do
      rem <- remainingArguments "args"
      action $ (\toParam -> do
        case parseMaybe parseProjectNameAndTags (T.pack $ unwords $ toParam rem) of
          Just (project, tags) -> do
            now <- dateCurrent
            appendWorkFlowEntry journalFile (FrameStart project tags now)
          Nothing -> error "Unable to parse start of frame")
    command "stop" $ do
      rem <- remainingArguments "args"
      action $ (\toParam -> do
        now <- dateCurrent
        (WorkFlow wfes) <- loadWorkFlowFromFile journalFile
        case headMay (reverse wfes) of
          Just (FrameStart project _ _) -> 
            appendWorkFlowEntry journalFile (FrameStop project now)
          _ -> error "Last entry is not start")
    command "log" $ do
      combined <- flag $ FlagShort 'c' <> FlagLong "combined"
      action $ (\toParam -> do
        wf@(WorkFlow wfes) <- loadWorkFlowFromFile journalFile
        if not (toParam combined)
          then mapM_ print wfes
          else do
           print $ vsep . fmap ppFrame $ fst $ combineFrames wf)

ppTime :: Maybe DateTime -> T.Text
ppTime mdt = case mdt of 
  Just dt -> T.pack $ timePrint timeFormat dt
  Nothing -> missing
  where
    timeFormat :: String
    timeFormat = "YYYY/MM/DD H:MI:S"
    missing = "-----------------"

ppTimeDiff :: Seconds -> Doc ann
ppTimeDiff totalSeconds = hsep $ fmap (pretty . T.pack) . catMaybes $ [ maybeZero h, maybeZero m, Just (show s) ]
  where
    d = fst $ fromSeconds totalSeconds
    h = durationHours d
    m = durationMinutes d
    s = durationSeconds d
    maybeZero z = if z > 0 then Just (show z) else Nothing

ppFrame :: Frame -> Doc ann
ppFrame f = case (fStart f, fEnd f) of
  (Just start, Just end) -> "["
    <+> pretty (ppTime (fStart f))
    <+> "->"
    <+> pretty (ppTime (fEnd f))
    <+> "]"
    <+> "("
    <+> ppTimeDiff (end `timeDiff` start)
    <+> ")"
    <+> pretty (fProject f)
    <+> (brackets $ hsep . fmap pretty $ (fTags f))
  _ -> "[" <+> pretty (ppTime (fStart f)) <+> "->" <+> pretty (ppTime (fEnd f)) <+> "]" <+> pretty (fProject f)

