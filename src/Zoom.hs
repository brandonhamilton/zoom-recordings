{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-----------------------------------------------------------
-- Zoom Recordings                                       --
-- Author: Brandon Hamilton <brandon.hamilton@gmail.com> --
-----------------------------------------------------------

module Zoom
  ( Recording(..)
  , Meeting(..)
  , Recordings(..)
  , RecordingStatus(..)
  , getRecordings
  , deleteRecording
  ) where

import Control.Monad.Time (currentTime)
import qualified Crypto.JOSE as Jose
import qualified Crypto.JWT as Jose
import Data.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToEncoding, genericToJSON, defaultOptions, constructorTagModifier)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Char (toLower)
import Data.Maybe (maybe)
import Data.Time.Calendar (addDays)
import Data.Time.Clock (UTCTime(..), addUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Lib (Application, Config(..))
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Relude
import Relude.Extra.Lens
import Servant.API
import Servant.Client
import Servant.Auth
import Servant.Auth.Client

data RecordingStatus = Completed | Processing
  deriving (Generic, Show)

instance FromJSON RecordingStatus where
  parseJSON = genericParseJSON $ defaultOptions { constructorTagModifier = fmap toLower}

instance ToJSON RecordingStatus where
  toJSON     = genericToJSON $ defaultOptions { constructorTagModifier = fmap toLower}
  toEncoding = genericToEncoding $ defaultOptions { constructorTagModifier = fmap toLower}

data Recording = Recording
  { recordingId :: Maybe Text
  , recordingMeetingId :: Text
  , recordingRecordingStart :: Text
  , recordingRecordingEnd :: Text
  , recordingFileType :: Text
  , recordingFileSize :: Maybe Integer
  , recordingPlayUrl :: Maybe Text
  , recordingDownloadUrl :: Text
  , recordingStatus :: Maybe RecordingStatus
  } deriving (Generic, Show)

instance FromJSON Recording where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON Recording where
  toJSON     = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

data Meeting = Meeting
  { meetingUuid :: Text
  , meetingId :: Integer
  , meetingAccountId :: Text
  , meetingHostId :: Text
  , meetingTopic :: Text
  , meetingType :: Integer
  , meetingStartTime :: Text
  , meetingTimezone :: Text
  , meetingHostEmail :: Text
  , meetingDuration :: Integer
  , meetingTotalSize :: Integer
  , meetingShareUrl :: Text
  , meetingRecordingCount :: Integer
  , meetingRecordingFiles :: [Recording]
  } deriving (Generic, Show)

instance FromJSON Meeting where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON Meeting where
  toJSON     = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

data Recordings = Recordings
  { recordingsFrom :: Text
  , recordingsTo :: Text
  , recordingsPageSize :: Integer
  , recordingsNextPageToken :: Text
  , recordingsMeetings :: [Meeting]
  } deriving (Generic, Show)

instance FromJSON Recordings where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance ToJSON Recordings where
  toJSON     = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

type ZoomRecordings = Auth '[JWT] () :>
      ( -- Get Account Recordings
           "accounts" :> "me" :> "recordings" :> QueryParam "page_size" Integer :> QueryParam "from" Text :> QueryParam "to" Text :> Get '[JSON] Recordings
        -- Delete meeting recording
      :<|> "meetings" :> Capture "meetingId" String  :> "recordings" :> Capture "recordingId" String :> QueryParam "action" String :> DeleteNoContent
      )

zoomRecordingsAPI :: Proxy ZoomRecordings
zoomRecordingsAPI = Proxy

zoomRecordings :: ClientEnv -> Client Application ZoomRecordings
zoomRecordings env = hoistClient zoomRecordingsAPI transform (client zoomRecordingsAPI)
  where
    transform :: ClientM a -> Application a
    transform = fmap (either (error . show) Relude.id) . liftIO . flip runClientM env

-- | Generate signed JWT token
generateToken :: Text -> ByteString -> IO (Either Jose.Error Token)
generateToken apiKey secret = runExceptT $ do
    c <- claims
    jwt <- Jose.signClaims (Jose.fromOctets secret) (Jose.newJWSHeader ((), Jose.HS256)) c
    pure $ Token $ toStrict $ Jose.encodeCompact jwt
  where
    claims = do
      t <- currentTime
      pure $ Jose.emptyClaimsSet
        & Jose.claimIss .~ Just (fromString . toString $ apiKey)
        & Jose.claimExp .~ Just (Jose.NumericDate (addUTCTime 60 t))

zoomBaseUrl :: BaseUrl
zoomBaseUrl = BaseUrl Https "api.zoom.us" 443 "v2"

-- | Get all cloud recordings for the master account
getRecordings :: Maybe Text -> Maybe Text -> Application (Either Text Recordings)
getRecordings startDate endDate = do
  cfg <- ask
  token <- liftIO $ generateToken (apiKey cfg) (encodeUtf8 . apiSecret $ cfg)
  case token of
    Left err -> pure . Left . show $ err
    Right token' -> do
      manager <- liftIO $ newManager tlsManagerSettings
      now <- currentTime
      let getAccountRecordings :: Maybe Integer -> Maybe Text -> Maybe Text -> Application Recordings
          getAccountRecordings :<|> _ = zoomRecordings (mkClientEnv manager zoomBaseUrl) token'
      Right <$> getAccountRecordings (Just 300) (startDate <|> from now) (endDate <|> to now)
  where
    -- from one month ago
    from :: UTCTime -> Maybe Text
    from d = Just $ toText $ formatTime defaultTimeLocale "%F" (d { utctDay = addDays (-30) (utctDay d) })
    -- until today
    to :: UTCTime -> Maybe Text
    to d = Just $ toText $ formatTime defaultTimeLocale "%F" d

-- | Delete a recording from the Zoom Cloud
deleteRecording :: Text -> Text -> Application (Either Text NoContent)
deleteRecording meetingId recordingId = do
  cfg <- ask
  liftIO $ putTextLn $ "Deleting recording " <> recordingId <> " from meeting " <> meetingId
  token <- liftIO $ generateToken (apiKey cfg) (encodeUtf8 . apiSecret $ cfg)
  case token of
    Left err -> pure . Left . show $ err
    Right token' -> do
      manager <- liftIO $ newManager tlsManagerSettings
      let deleteAccountRecording :: String -> String -> Maybe String -> Application NoContent
          _ :<|> deleteAccountRecording = zoomRecordings (mkClientEnv manager zoomBaseUrl) token'
      Right <$> deleteAccountRecording (toString meetingId) (toString recordingId) (Just "trash")
