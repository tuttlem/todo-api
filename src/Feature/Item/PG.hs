module Feature.Item.PG where

import ClassyPrelude

import Feature.Item.Types
import Feature.Auth.Types
import Platform.PG
import Database.PostgreSQL.Simple

listItemsByUser :: PG r m => UserId -> m [Item]
listItemsByUser uId = withConn $ \conn -> query conn qry (Only uId)
    where
        qry = "select description, done \
              \from items \
              \where user_id = ?"

