-----------------------------------------------------------
-- Zoom Recordings                                       --
-- Author: Brandon Hamilton <brandon.hamilton@gmail.com> --
-----------------------------------------------------------

module Lib
  ( Application(..)
  , Config(..)
  ) where

import Control.Monad.Time (MonadTime)
import Relude
import System.Envy

data Config = Config
  { accountId :: Text
  -- ^ Zoom API OAuth Account ID
  , clientId :: Text
  -- ^ Zoom API OAuth Client ID
  , clientSecret :: Text
  -- ^ Zoom API  OAuth Client Secret
  , dir :: FilePath
  -- ^ Destination directory
  } deriving (Generic, Show)
    deriving anyclass FromEnv

newtype Application a = Application { runApplication :: ReaderT Config IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Config, MonadTime)
