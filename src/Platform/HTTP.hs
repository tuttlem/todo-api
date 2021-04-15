module Platform.HTTP (
    main
) where

import ClassyPrelude

import Data.Default
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import Network.Wai (Response, Middleware)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger 
import Network.Wai.Middleware.RequestLogger.JSON

import qualified Feature.Auth.HTTP as Auth
import qualified Feature.Version.HTTP as Ver
import qualified Feature.User.HTTP as User
import qualified Feature.Item.HTTP as Item

import System.Environment

type App r m = (Auth.Service m, Ver.Service m, User.Service m, Item.Service m, MonadIO m)

jsonRequestLogger :: (MonadIO m) => m Middleware
jsonRequestLogger =
    liftIO $ mkRequestLogger $ def { outputFormat = CustomOutputFormatWithDetails formatAsJSON }

main :: (App r m) => (m Response -> IO Response) -> IO ()
main runner = do
    port <- acquirePort
    mayTLSSetting <- acquireTLSSetting
    logger <- jsonRequestLogger

    case mayTLSSetting of
      Nothing ->
          scottyT port runner $ routes logger
      Just tlsSetting -> do
          app <- scottyAppT runner $ routes logger
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

routes :: (App r m) => Middleware -> ScottyT LText m ()
routes logger = do
    -- middlewares
    middleware logger

    -- middleware logStdoutDev
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
    Ver.routes
    User.routes
    Item.routes
