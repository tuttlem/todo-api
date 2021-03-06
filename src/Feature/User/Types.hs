module Feature.User.Types where

import ClassyPrelude

import Feature.Auth.Types
import Database.PostgreSQL.Simple.FromRow
import Platform.AesonUtil

type Username = Text
type Password = Text
type Email = Text

data Auth = Auth
    { authEmail :: Email
    , authPassword :: Password
    } deriving (Eq, Show)

data Register = Register
    { registerUsername :: Username
    , registerEmail :: Email
    , registerPassword :: Text
    } deriving (Show)

data UpdateUser = UpdateUser
    { updateUserEmail :: Maybe Email
    , updateUserUsername :: Maybe Username
    , updateUserPassword :: Maybe Password
    , updateUserImage :: Maybe Text
    , updateUserBio :: Maybe Text
    } deriving (Eq, Show)

data User = User
    { userEmail :: Email
    , userToken :: Token
    , userUsername :: Username
    , userBio :: Text
    , userImage :: Text
    } deriving (Eq, Show)

data Profile = Profile
    { profileUsername :: Username
    , profileBio :: Text
    , profileImage :: Text
    , profileFollowing :: Bool
    } deriving (Eq, Show)

data UserError
    = UserErrorBadAuth Auth
    | UserErrorNotFound Username
    | UserErrorNameTaken Username
    | UserErrorEmailTaken Email
    deriving (Eq, Show)

newtype UserWrapper a = UserWrapper { userWrapperUser :: a } deriving (Eq, Show)
newtype ProfileWrapper a = ProfileWrapper { profileWrapperProfile :: a } deriving (Eq, Show)

$(commonJSONDeriveMany
    [ ''User
    , ''Profile
    , ''UserError
    , ''ProfileWrapper
    , ''UserWrapper
    , ''Auth
    , ''Register
    , ''UpdateUser
    ])

instance FromRow Profile where
    fromRow =  Profile 
           <$> field 
           <*> field 
           <*> field 
           <*> field
