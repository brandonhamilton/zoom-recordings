-----------------------------------------------------------
-- Zoom Recordings                                       --
-- Author: Brandon Hamilton <brandon.hamilton@gmail.com> --
-----------------------------------------------------------

module Main where

import Control.Applicative
import Control.Monad.Time (currentTime)
import Data.Text (pack)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Lib
import Options
import Recordings
import Relude
import System.Envy
import Zoom

data MainOptions = MainOptions
    { optDeleteRecordings :: Bool
    , optStartDate :: Maybe String
    , optEndDate :: Maybe String
    }

instance Options MainOptions where
  defineOptions = MainOptions
      <$> simpleOption "delete" False
          "Delete recordings after downloading"
      <*> simpleOption "start" Nothing
          "Start date, eg: 2019-11-31"
      <*> simpleOption "end" Nothing
          "End date, eg: 2019-11-31"

main :: IO ()
main = runCommand $ \opts args -> do
  env <- decodeEnv :: IO (Either String Config)
  case env of
    Left err -> print err
    Right cfg -> flip runReaderT cfg $ runApplication $ do
      now <- currentTime
      putTextLn $ "Start downloading Zoom recordings at " <> show now
      r <- getRecordings (pack <$> optStartDate opts) (pack <$> optEndDate opts)
      case r of
        Left err -> print err
        Right recs -> do
          done <- concat <$> traverse saveMeeting (recordingsMeetings recs)
          if optDeleteRecordings opts
          then do
            putTextLn $ "Deleting " <> (show . length $ done) <> " recordings"
            deleted <- traverse (uncurry deleteRecording) done
            pure ()
          else putTextLn "Not deleting recordings after download"
          putTextLn "Completed"
          pure ()
