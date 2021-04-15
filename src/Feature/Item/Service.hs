module Feature.Item.Service where

import ClassyPrelude

-- import Control.Monad.Except
import Feature.Item.Types
-- import Feature.User.Types
import Feature.Auth.Types
-- import Feature.Common.Util (orThrow)

listItems :: (ItemRepo m) => CurrentUser -> m [Item]
listItems (_, userId) = listItemsByUser userId

class (Monad m) => ItemRepo m where
  listItemsByUser :: UserId -> m [Item]

