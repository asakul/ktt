{-# LANGUAGE OverloadedStrings #-}

module Productivity.TimeTracking.KTT (
  Frame(..),
  WorkFlow(..),
  WorkFlowEntry(..),
  parseWorkFlowEntry,
  loadWorkFlowFromFile
) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.Binary as CB
import           Data.Conduit.List as CL
import           Data.Conduit.Text as CT
import           Data.List.NonEmpty
import qualified Data.Text as T
import           Time.Types
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L

data Frame = Frame {
  fProject :: T.Text,
  fTags :: [T.Text],
  fStart :: Maybe DateTime,
  fEnd :: Maybe DateTime
} deriving (Show, Eq)

data WorkFlow = WorkFlow [Frame]
  deriving (Show, Eq)

data WorkFlowEntry = FrameStart T.Text [T.Text] DateTime | FrameStop T.Text DateTime
  deriving (Show, Eq)

data ActionToken = Start | Stop
  deriving (Show, Eq)

loadWorkFlowFromFile :: FilePath -> IO [WorkFlowEntry]
loadWorkFlowFromFile filename = do
  runResourceT . runConduit $
    CB.sourceFile filename =$=
    CT.decode CT.utf8 =$=
    CT.lines =$=
    parserConduit =$=
    CL.consume
  where
    parserConduit :: Conduit T.Text (ResourceT IO) WorkFlowEntry
    parserConduit = do
      c <- await
      case c of
        Just input -> 
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

    parseProjectNameAndTags = do
      pn <- parseProjectName
      tags <- parseTags
      return (pn, tags)

    parseProjectName = do
      projectWords <- some projectWord
      return $ T.unwords projectWords

    projectWord = do
      first <- noneOf ['+']
      rest <- many (alphaNumChar <|> symbolChar)
      skipMany separatorChar
      return $ T.pack (first : rest)

    parseTags = do
      tags <- many parseTag
      return tags

    parseTag = do
      void $ char '+'
      tag <- some (alphaNumChar <|> symbolChar)
      skipMany separatorChar
      return $ T.pack tag

        
parseWorkFlowEntry :: T.Text -> Either T.Text WorkFlowEntry
parseWorkFlowEntry input = case parse workFlowEntryParser "" input of
  Right a -> Right a
  Left err -> Left $ T.pack $ "error: " ++ show err