{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Console.Options
import           Data.Hourglass
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Text.Megaparsec
import           Text.Tabl
import           Time.System

import           Safe
import           System.Directory
import           System.Environment

import           Productivity.TimeTracking.KTT

getJournalFilename = do
  e <- lookupEnv "KTT_JOURNAL_FILE"
  case e of
    Just fname -> return fname
    _          -> getXdgDirectory XdgData "ktt.journal"

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
            let hdecor = DecorNone
            let vdecor = DecorAll
            let aligns = [AlignCentre, AlignCentre, AlignRight ]
            let frames = fst $ combineFrames wf
            let cells = fmap createFrameRow frames
            TIO.putStrLn $ tabl EnvAscii hdecor vdecor aligns cells)
    command "report" $
      action (\toParam -> do
        frames <- fst . combineFrames <$> loadWorkFlowFromFile journalFile
        let hdecor = DecorNone
        let vdecor = DecorAll
        let aligns = [AlignCentre]
        let cells = fmap createReportRow $ M.toList $ foldl foldFrames M.empty frames
        TIO.putStrLn $ tabl EnvAscii hdecor vdecor aligns cells)

createFrameRow :: Frame -> [T.Text]
createFrameRow frame = [ppTime (fStart frame), ppTime (fEnd frame), durationFrame, fProject frame, T.unwords (fTags frame)]
  where
    durationFrame = case (fStart frame, fEnd frame) of
      (Just start, Just end) -> ppTimeDiff $ end `timeDiff` start
      _                      -> " *** "

createReportRow :: (T.Text, Seconds) -> [T.Text]
createReportRow (project, time) = [project, ppTimeDiff time]

foldFrames :: M.Map T.Text Seconds -> Frame -> M.Map T.Text Seconds
foldFrames m f = case (fStart f, fEnd f) of
  (Just start, Just end) -> M.alter (\mx -> case mx of
    Just x  -> Just $ x + end `timeDiff` start
    Nothing -> Just $ end `timeDiff` start) (fProject f) m
  _ -> m

ppTime :: Maybe DateTime -> T.Text
ppTime mdt = case mdt of
  Just dt -> T.pack $ timePrint timeFormat dt
  Nothing -> missing
  where
    timeFormat :: String
    timeFormat = "YYYY/MM/DD H:MI:S"
    missing = "-------------------"

ppTimeDiff :: Seconds -> T.Text
ppTimeDiff totalSeconds = T.unwords . fmap T.pack . catMaybes $ [ maybeZero h, maybeZero m, Just (show s) ]
  where
    d = fst $ fromSeconds totalSeconds
    h = durationHours d
    m = durationMinutes d
    s = durationSeconds d
    maybeZero z = if z > 0 then Just (show z) else Nothing

