module Feature.Version.HTTP
  ( routes
  , Service(..)
  ) where

import ClassyPrelude

import Feature.Version.Types
import Web.Scotty.Trans

class Monad m => Service m where
  getVersion :: m VersionInfo

routes :: (Service m, MonadIO m) => ScottyT LText m ()
routes = do

    -- health/info
    get "/ver" $ do
        versionInfo <- lift getVersion
        json versionInfo
