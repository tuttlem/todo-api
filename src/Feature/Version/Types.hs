module Feature.Version.Types where

import ClassyPrelude

import Platform.AesonUtil

data VersionInfo = VersionInfo
    { versionInfoName :: String
    , versionInfoVersion :: String
    } deriving (Eq, Show)

$(commonJSONDeriveMany
  [ ''VersionInfo
  ])
