module Feature.Item.HTTP 
  ( routes
  , Service(..)
  ) where

import ClassyPrelude

import Feature.Item.Types
import Feature.Auth.Types
-- import Feature.User.Types
-- import Feature.Common.HTTP
import qualified Feature.Auth.HTTP as Auth
import Web.Scotty.Trans
-- import Network.HTTP.Types.Status

class Monad m => Service m where
  listItems :: CurrentUser -> m [Item]

routes :: (Auth.Service m, Service m, MonadIO m) => ScottyT LText m ()
routes = do

  get "/items" $ do
    curUser <- Auth.requireUser
    result <- lift $ listItems curUser
    json $ ItemWrapper result
