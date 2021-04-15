module Feature.Item.Types where

import ClassyPrelude

import Database.PostgreSQL.Simple.FromRow
import Platform.AesonUtil

data Item = Item
    { itemDescription :: Text
    , itemDone :: Bool
    } deriving (Eq, Show)

newtype ItemWrapper a = ItemWrapper { itemWrapperItems :: a } deriving (Eq, Show)

$(commonJSONDeriveMany
    [ ''Item
    , ''ItemWrapper
    ])

instance FromRow Item where
    fromRow =  Item
           <$> field
           <*> field
