module Misc.Client where

import ClassyPrelude

import Feature.Common.Types
import Feature.Auth.Types
import Feature.Version.Types
import Network.Wreq hiding (Auth)
import Control.Monad.Except
import Control.Lens hiding ((.=))
import qualified Data.Aeson as Aeson
-- import Data.Aeson ((.=))
import Network.HTTP.Types.Status
import qualified Network.HTTP.Client as HC
import Data.Has

data Err a
  = ErrMalformedJSON Text
  | ErrInvalidInput InputViolations
  | ErrInternalServerError ByteString
  | ErrUnathorized TokenError
  | ErrApp a
  | ErrUnknown Text
  deriving (Eq, Show)

type RW r m = (MonadIO m, Has RWBaseUrl r, MonadReader r m, MonadUnliftIO m)

newtype RWBaseUrl = RWBaseUrl String

buildUrl :: (RW r m) => String -> ExceptT e m String
buildUrl path = do
    (RWBaseUrl baseUrl) <- asks getter
    return $ baseUrl <> path

-- 
-- Version endpoint
--
version :: (RW r m) => m (Either (Err Text) VersionInfo)
version = runExceptT $ do
    url <- buildUrl "/ver"
    exec $ get url


--
-- 
exec :: (RW r m, Aeson.FromJSON a, Aeson.FromJSON e) => IO (Response LByteString) -> ExceptT (Err e) m a
exec req = ExceptT $ do
    r <- liftIO (Right <$> (req >>= asJSON))
      `catch` handleHttpException
      `catch` handleJSONError
      `catch` handleOtherException
    return $ case r of
      Left err -> Left err
      Right r' -> Right $ r' ^. responseBody
    where
      handleJSONError (JSONError err) = return . Left $ ErrMalformedJSON $ tshow err
      handleHttpException (HC.HttpExceptionRequest _ (HC.StatusCodeException res body)) =
          let status = HC.responseStatus res
           in if status == status500 then
                return . Left $ ErrInternalServerError body
              else if status == status401 then
                parseJSONError body ErrUnathorized
              else if status == status422 then
                parseJSONError body (ErrInvalidInput . errorsWrapperErrors)
              else
                parseJSONError body ErrApp
      handleHttpException err = return . Left $ ErrUnknown $ tshow err
      handleOtherException (e :: SomeException) = return . Left $ ErrUnknown $ tshow e
      parseJSONError src f = case Aeson.eitherDecode $ fromStrict src of
        Left parseErr -> return . Left $ ErrMalformedJSON $ tshow parseErr
        Right parseResult -> return . Left $ f parseResult

authHeader :: Token -> Network.Wreq.Options -> Network.Wreq.Options
authHeader token = header "Authorization" .~ ["Token " <> fromString (unpack token)]

mayAuthHeader :: Maybe Token -> Network.Wreq.Options -> Network.Wreq.Options
mayAuthHeader = maybe id authHeader


