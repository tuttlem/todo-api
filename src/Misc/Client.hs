module Misc.Client where

-- import ClassyPrelude
--
-- import Feature.Common.Types
-- import Feature.Auth.Types
-- import Network.Wreq hiding (Auth)
-- import Control.Monad.Except
-- import Control.Lens hiding ((.=))
-- import qualified Data.Aeson as Aeson
-- import Data.Aeson ((.=))
-- import Network.HTTP.Types.Status
-- import qualified Network.HTTP.Client as HC
-- import Data.Has
--
-- data Err a
  -- = ErrMalformedJSON Text
  -- | ErrInvalidInput InputViolations
  -- | ErrInternalServerError ByteString
  -- | ErrUnathorized TokenError
  -- | ErrApp a
  -- | ErrUnknown Text
  -- deriving (Eq, Show)
--
-- type RW r m = (MonadIO m, Has RWBaseUrl r, MonadReader r m, MonadUnliftIO m)
--
-- newtype RWBaseUrl = RWBaseUrl String
--
-- buildUrl :: (RW r m) => String -> ExceptT e m String
-- buildUrl path = do
    -- (RWBaseUrl baseUrl) <- asks getter
    -- return $ baseUrl <> path
--
-- --
-- -- Version endpoint
-- --
-- version :: (RW r m) => m (Either (Err Text)
