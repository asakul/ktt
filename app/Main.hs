{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import           Console.Options
import           Control.Monad
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
    command "report" $ do
      week <- flag $ FlagShort 'w' <> FlagLong "week"
      today <- flag $ FlagShort 't' <> FlagLong "today"
      action (\toParam -> do
        when (toParam week && toParam today) $
          error "Either -w or -t should be active"
        now <- dateCurrent
        frames <- fst . combineFrames <$> loadWorkFlowFromFile journalFile
        let hdecor = DecorNone
        let vdecor = DecorAll
        let aligns = [AlignCentre]
        let cells = fmap createReportRow $ M.toList $ foldl foldFrames M.empty $ applyFilters now (toParam week) (toParam today) frames
        TIO.putStrLn $ tabl EnvAscii hdecor vdecor aligns cells)

applyFilters :: DateTime -> Bool -> Bool -> [Frame] -> [Frame]
applyFilters now onlyWeek onlyToday frames = case extents of
  Just e -> filter (isInTimeExtents e) frames
  Nothing -> frames
  where
    isInTimeExtents extents@(start, end) frame = case fEnd frame of
      Just frameEnd -> fStart frame >= start && frameEnd <= end
      Nothing -> fStart frame >= start && fStart frame <= end
    extents = if
      | onlyWeek -> Just (startOfWeek, endOfWeek)
      | onlyToday -> Just (startOfToday, endOfToday)
      | otherwise -> Nothing

    startOfToday = now { dtTime = midnight }
    endOfToday = now { dtDate = dtDate now `dateAddPeriod` oneDayPeriod, dtTime = midnight }
    oneDayPeriod = Period 0 0 1

    startOfWeek = DateTime { dtDate = dtDate now `dateAddPeriod` negDelta, dtTime = midnight }
    endOfWeek = DateTime { dtDate = dtDate now `dateAddPeriod` posDelta, dtTime = midnight }
    midnight = TimeOfDay 0 0 0 0
    posDelta = if (getWeekDay . dtDate) now /= Sunday
      then Period 0 0 (7 - fromEnum (getWeekDay . dtDate $ now) + 1)
      else Period 0 0 1
    negDelta = if (getWeekDay . dtDate) now /= Sunday
      then Period 0 0 (-(fromEnum (getWeekDay . dtDate $ now) - 1))
      else Period 0 0 7

createFrameRow :: Frame -> [T.Text]
createFrameRow frame = [ppTime (Just $ fStart frame), ppTime (fEnd frame), durationFrame, fProject frame, T.unwords (fTags frame)]
  where
    durationFrame = case fEnd frame of
      (Just end) -> ppTimeDiff $ end `timeDiff` fStart frame
      _                      -> " *** "

createReportRow :: (T.Text, Seconds) -> [T.Text]
createReportRow (project, time) = [project, ppTimeDiff time]

foldFrames :: M.Map T.Text Seconds -> Frame -> M.Map T.Text Seconds
foldFrames m f = case fEnd f of
  Just end -> M.alter (\mx -> case mx of
    Just x  -> Just $ x + end `timeDiff` fStart f
    Nothing -> Just $ end `timeDiff` fStart f) (fProject f) m
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

