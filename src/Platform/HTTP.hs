module Platform.HTTP (
    main
) where

import ClassyPrelude

import Web.Scotty.Trans
import Network.HTTP.Types.Status
import Network.Wai (Response)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Middleware.Cors

import System.Environment

import Data.Version
import qualified Paths_todo_api
import Platform.AesonUtil

type App r m = (MonadIO m)

data VersionInfo = VersionInfo
    { versionInfoName :: String
    , versionInfoVersion :: String
    } deriving (Eq, Show)

$(commonJSONDeriveMany
  [ ''VersionInfo
  ])

getVersionInfo :: IO VersionInfo
getVersionInfo = do
    name <- getProgName
    return VersionInfo { versionInfoName = name
                       , versionInfoVersion = versionStr
                       }
  where versionStr = showVersion Paths_todo_api.version


main :: (App r m) => (m Response -> IO Response) -> IO ()
main runner = do
    port <- acquirePort
    mayTLSSetting <- acquireTLSSetting

    case mayTLSSetting of
      Nothing ->
          scottyT port runner routes
      Just tlsSetting -> do
          app <- scottyAppT runner routes
          runTLS tlsSetting (setPort port defaultSettings) app
    where
        acquirePort = do
            port <- fromMaybe "" <$> lookupEnv "PORT"
            return . fromMaybe 3000 $ readMay port
        acquireTLSSetting = do
            env <- (>>= readMay) <$> lookupEnv "ENABLE_HTTPS"
            let enableHttps = fromMaybe True env
            return $ if enableHttps
                        then Just $ tlsSettings "secrets/tls/certificate.pem" "secrets/tls/key.pem"
                        else Nothing

routes :: (App r m) => ScottyT LText m ()
routes = do
    -- middlewares
    middleware $ cors $ const $ Just simpleCorsResourcePolicy
        { corsRequestHeaders = "Authorization":simpleHeaders
        , corsMethods = "PUT":"DELETE":simpleMethods
        }
    options (regex ".*") $ return ()

    -- error handler
    defaultHandler $ \str -> do
        status status500
        json str


    -- TODO: fill in feature routes here
    --

    -- health/info
    get "/ver" $ do
        versionInfo <- liftIO getVersionInfo
        json versionInfo


