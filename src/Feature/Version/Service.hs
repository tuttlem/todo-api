module Feature.Version.Service where

import ClassyPrelude
import System.Environment
import Data.Version
import qualified Paths_todo_api
import Feature.Version.Types

getVersion :: (MonadIO m) => m VersionInfo
getVersion = do
    name <- liftIO getProgName
    return VersionInfo { versionInfoName = name
                       , versionInfoVersion = versionStr
                       }
  where versionStr = showVersion Paths_todo_api.version

