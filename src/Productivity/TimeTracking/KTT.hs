{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Productivity.TimeTracking.KTT (
  Frame(..),
  WorkFlow(..),
  WorkFlowEntry(..),
  parseWorkFlowEntry,
  parseProjectNameAndTags,
  parseProjectName,
  parseTags,
  loadWorkFlowFromFile,
  combineFrames,
  renderWorkFlowEntry,
  appendWorkFlowEntry
) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Resource
import           Control.Monad.Writer
import           Data.Char
import           Data.Conduit
import           Data.Conduit.Binary as CB
import           Data.Conduit.List as CL
import           Data.Conduit.Text as CT
import           Data.Hourglass
import qualified Data.Map as M
import           Data.List.NonEmpty hiding (reverse)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.IO as TIO
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           System.IO

data Frame = Frame {
  fProject :: T.Text,
  fTags :: [T.Text],
  fStart :: Maybe DateTime,
  fEnd :: Maybe DateTime
} deriving (Show, Eq)

data WorkFlow = WorkFlow [WorkFlowEntry]
  deriving (Show, Eq)

data WorkFlowEntry = FrameStart T.Text [T.Text] DateTime | FrameStop T.Text DateTime
  deriving (Show, Eq)

renderWorkFlowEntry :: WorkFlowEntry -> T.Text
renderWorkFlowEntry entry = TL.toStrict . TLB.toLazyText . mconcat $ case entry of
  FrameStart project tags timestamp ->
    [ TLB.fromText . T.pack $ timePrint timeFormat timestamp,
      TLB.singleton ' ',
      TLB.fromText "START",
      TLB.singleton ' ',
      TLB.fromText project,
      TLB.singleton ' ',
      TLB.fromText . T.unwords . fmap (T.cons '+') $ tags ]
  FrameStop project timestamp ->
    [ TLB.fromText . T.pack $ timePrint timeFormat timestamp,
      TLB.singleton ' ',
      TLB.fromText "STOP",
      TLB.singleton ' ',
      TLB.fromText project ]
  where
    timeFormat :: String
    timeFormat = "YYYY/MM/DD H:MI:S"

data ActionToken = Start | Stop
  deriving (Show, Eq)

appendWorkFlowEntry :: FilePath -> WorkFlowEntry -> IO ()
appendWorkFlowEntry filename entry =
  withFile filename ReadWriteMode (\h -> do
    hSeek h SeekFromEnd 1
    isEnd <- hIsEOF h
    when (not isEnd) $ do
      lastChar <- hGetChar h
      hSeek h SeekFromEnd 0
      when (lastChar /= '\n') $ hPutStrLn h ""
    hSeek h SeekFromEnd 0
    TIO.hPutStrLn h $ renderWorkFlowEntry entry)

combineFrames :: WorkFlow -> ([Frame], [T.Text])
combineFrames (WorkFlow entries) = runWriter $ combineFrames' entries M.empty []
  where
    combineFrames' (e:es) frames acc = case e of
      FrameStart project tags timestamp -> do
        when (M.member project frames) $ tell ["Project reopened: " `T.append` project]
        combineFrames' es (M.insert project (Frame project tags (Just timestamp) Nothing) frames) acc
      FrameStop project timestamp -> do
        case M.lookup project frames of
          Just frame -> combineFrames' es (M.delete project frames) (frame { fEnd = Just timestamp } : acc)
          Nothing -> do
            tell ["Unopened project is closed: " `T.append` project]
            combineFrames' es frames acc
    combineFrames' [] frames acc = return $ (fmap snd . M.toList) frames ++ acc

loadWorkFlowFromFile :: FilePath -> IO WorkFlow
loadWorkFlowFromFile filename = do
  f <- runResourceT . runConduit $
    CB.sourceFile filename =$=
    CT.decode CT.utf8 =$=
    CT.lines =$=
    parserConduit =$=
    CL.consume
  return $ WorkFlow f
  where
    parserConduit :: Conduit T.Text (ResourceT IO) WorkFlowEntry
    parserConduit = do
      c <- await
      case c of
        Just input -> do
          case parseWorkFlowEntry input of
            Left _ -> parserConduit
            Right entry -> do
              yield entry
              parserConduit
        _ -> return ()

workFlowEntryParser :: Parsec () T.Text WorkFlowEntry
workFlowEntryParser = do
  timestamp <- parseTimestamp
  startOrStop <- parseStartOrStop
  case startOrStop of
    Start -> do
      (projectName, tags) <- parseProjectNameAndTags
      return $ FrameStart projectName tags timestamp
    Stop -> do
      projectName <- parseProjectName
      return $ FrameStop projectName timestamp

  where
    parseTimestamp = do
      year <- fromInteger <$> L.decimal
      void $ char '/'
      month <- fromInteger <$> L.decimal
      when (month < 1 || month > 12) $
        unexpected (Tokens $ fromList $ show month)
      void $ char '/'
      day <- fromInteger <$> L.decimal
      skipSome spaceChar
      hour <- fromInteger <$> L.decimal
      void $ char ':'
      minute <- fromInteger <$> L.decimal
      void $ char ':'
      second <- fromInteger <$> L.decimal
      skipSome spaceChar
      return $ DateTime (Date year (toEnum (month - 1)) day) (TimeOfDay hour minute second 0)
     
    parseStartOrStop = do
      s <- string' "START" <|> string' "STOP"
      skipSome spaceChar
      case T.toUpper . T.pack $ s of
        "START" -> return Start
        "STOP" -> return Stop
        _ -> unexpected (Tokens $ fromList s)

parseProjectNameAndTags :: Parsec () T.Text (T.Text, [T.Text])
parseProjectNameAndTags = do
  pn <- parseProjectName
  tags <- parseTags
  return (pn, tags)

parseProjectName :: Parsec () T.Text T.Text
parseProjectName = do
  projectWords <- some projectWord
  return $ T.unwords projectWords
  where
    projectWord = do
      first <- satisfy (\x -> (not . isSpace) x && x /= '+')
      rest <- many (alphaNumChar <|> symbolChar <|> punctuationChar)
      skipMany separatorChar
      return $ T.pack (first : rest)

parseTags :: Parsec () T.Text [T.Text]
parseTags = do
  tags <- many parseTag
  return tags
  where
    parseTag = do
      void $ char '+'
      tag <- some (alphaNumChar <|> symbolChar <|> punctuationChar)
      skipMany separatorChar
      return $ T.pack tag
        
parseWorkFlowEntry :: T.Text -> Either T.Text WorkFlowEntry
parseWorkFlowEntry input = case parse workFlowEntryParser "" input of
  Right a -> Right a
  Left err -> Left $ T.pack $ "error: " ++ show err
