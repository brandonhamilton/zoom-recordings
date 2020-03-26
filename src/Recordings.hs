{-# LANGUAGE RankNTypes            #-}

-----------------------------------------------------------
-- Zoom Recordings                                       --
-- Author: Brandon Hamilton <brandon.hamilton@gmail.com> --
-----------------------------------------------------------

module Recordings
  ( saveMeeting
  ) where

import Conduit (throwM)
import Control.Exception (try)
import Data.Conduit ((.|), runConduitRes)
import Data.Conduit.Binary (sinkFile)
import Data.Maybe (fromJust)
import Data.Text (replace, toLower, unpack)
import Data.Yaml
import Lib
import Network.HTTP.Req
import Network.HTTP.Req.Conduit
import Relude
import System.Directory
import System.FilePath
import System.IO
import Text.URI (mkURI)
import Zoom

data DownloadException = FileSizeError Int Integer
  deriving (Show, Typeable)
instance Exception DownloadException

downloadFile :: FilePath -> FilePath -> Text -> Int -> Application (Either SomeException ())
downloadFile dir file uri expectedFileSize = liftIO $ try $ do
    -- Ensure destination directory exists
    createDirectoryIfMissing True dir
    -- Download to temporary file
    let dest = dir </> file
        tmp = dest <.> "tmp"
    putTextLn $ "Downloading " <> toText file <> " (" <> show expectedFileSize <> " bytes)"
    mUri <- mkURI uri
    let (url, options) = fromJust (useHttpsURI mUri)
    runReq defaultHttpConfig $ reqBr GET url NoReqBody options $ \r -> runConduitRes $ responseBodySource r .| sinkFile tmp
    -- Check that file was fully downloaded
    checkFileSize tmp
    -- Rename temporary file
    renameFile tmp dest
    pure ()
  where
    checkFileSize :: FilePath -> IO ()
    checkFileSize dest = withBinaryFile dest ReadMode $ \f -> do
      size <- hFileSize f
      when (size > toInteger (maxBound :: Int)) $ throwM $ FileSizeError expectedFileSize size
      when (fromInteger size /= expectedFileSize) $ throwM $ FileSizeError expectedFileSize size

processRecording :: FilePath -> Recording -> Application (Either Text Text)
processRecording dir rec = case recordingStatus rec of
  Nothing -> pure $ Left "Not a media file"
  Just Processing -> pure $ Left "File is still being processed"
  Just Completed ->
    case recordingId rec of
      Nothing -> pure $ Left "No recording id"
      Just recId -> do
        let file = toString recId <.> (toString . toLower . recordingFileType $ rec)
        let fileSize = fromJust . recordingFileSize $ rec
        dl <- downloadFile dir file (recordingDownloadUrl rec) (fromIntegral fileSize)
        case dl of
          Left err -> pure $ Left (show err)
          Right res -> do
            liftIO $ putTextLn $ "Successfully downloaded file " <> toText file <> " (" <> show fileSize <> " bytes)"
            pure $ Right recId

-- | Download all recordings for a meeting
saveMeeting :: Meeting -> Application [(Text, Text)]
saveMeeting m = do
    d <- asks dir
    let meetingIdentity = show . meetingId $ m
        meetingDir = d </> toString (makeValid . unpack $ description)
        description = replace "Z" "" (replace ":" "-" (replace "T" " - " $ meetingStartTime m)) <> " - " <> replace "/" "-" (meetingTopic m)
    liftIO $ putTextLn $ "Processing recording from " <> meetingTopic m <> " on " <> (show . meetingStartTime $ m) <> " (" <> meetingIdentity <> ")"
    liftIO $ createDirectoryIfMissing True meetingDir
    liftIO $ encodeFile (meetingDir </> "meeting.yaml") m
    recs <- traverse (processRecording meetingDir) (meetingRecordingFiles m)
    liftIO $ putTextLn $ "Downloaded " <> (show . length . rights $ recs) <> "/" <> (show . length $ recs) <> " recordings for " <> meetingIdentity
    pure $ (,) meetingIdentity <$> rights recs
