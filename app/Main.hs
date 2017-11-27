{-# LANGUAGE OverloadedStrings #-}

module Main where

import           UI.Butcher.Monadic
import           Data.Hourglass
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Monoid
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty            as NE
import           Data.List.Split
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
  case splitOn ";" <$> e of
    Just (activeFname:rest) -> return (activeFname :| rest)
    _          -> do
      dir <- getXdgDirectory XdgData "ktt.journal"
      return $ dir :| []

main :: IO ()
main = do
  journalFiles <- getJournalFilename
  let mainFile = NE.head journalFiles
  let auxFiles = NE.tail journalFiles
  mainFromCmdParserWithHelpDesc $ \helpDesc -> do
    addCmdSynopsis "K time tracker"
    addCmd "start" $ do
      rem <- addParamRestOfInput "STRING" (paramHelpStr "Project name with +tags")
      addCmdImpl $ do
        case parseMaybe parseProjectNameAndTags (T.pack $ rem) of
          Just (project, tags) -> do
            now <- dateCurrent
            appendWorkFlowEntry mainFile (FrameStart project tags now)
          Nothing -> error "Unable to parse start of frame"
    addCmd "stop" $ do
      rem <- addParamRestOfInput "STRING" (paramHelpStr "Project name")
      addCmdImpl $ do
        now <- dateCurrent
        (WorkFlow wfes) <- loadWorkFlowFromFiles (NE.toList journalFiles)
        case headMay (reverse wfes) of
          Just (FrameStart project _ _) ->
            appendWorkFlowEntry mainFile (FrameStop project now)
          _ -> error "Last entry is not start"
    addCmd "log" $ do
      combined <- addSimpleBoolFlag "" ["combined"] (flagHelpStr "Combined mode")
      addCmdImpl $ do
        wf@(WorkFlow wfes) <- loadWorkFlowFromFiles (NE.toList journalFiles)
        if not combined
          then mapM_ print wfes
          else do
            let hdecor = DecorNone
            let vdecor = DecorAll
            let aligns = [AlignCentre, AlignCentre, AlignRight ]
            let frames = fst $ combineFrames wf
            let cells = fmap createFrameRow frames
            TIO.putStrLn $ tabl EnvAscii hdecor vdecor aligns cells
    addCmd "report" $
      addCmdImpl $ do
        frames <- fst . combineFrames <$> loadWorkFlowFromFiles (NE.toList journalFiles)
        let hdecor = DecorNone
        let vdecor = DecorAll
        let aligns = [AlignCentre]
        let cells = fmap createReportRow $ M.toList $ foldl foldFrames M.empty frames
        TIO.putStrLn $ tabl EnvAscii hdecor vdecor aligns cells

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

