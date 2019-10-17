-----------------------------------------------------------
-- Zoom Recordings                                       --
-- Author: Brandon Hamilton <brandon.hamilton@gmail.com> --
-----------------------------------------------------------

module Main where

import Control.Monad.Time (currentTime)
import Lib
import Recordings
import Relude
import System.Envy
import Zoom

main :: IO ()
main = do
  env <- decodeEnv :: IO (Either String Config)
  case env of
    Left err -> print err
    Right cfg -> flip runReaderT cfg $ runApplication $ do
      now <- currentTime
      putTextLn $ "Start downloading Zoom recordings at " <> show now
      r <- getRecordings
      case r of
        Left err -> print err
        Right recs -> do
          done <- concat <$> traverse saveMeeting (recordingsMeetings recs)
          putTextLn $ "Deleting " <> (show . length $ done) <> " recordings"
          deleted <- traverse (uncurry deleteRecording) done
          putTextLn "Completed"
          pure ()