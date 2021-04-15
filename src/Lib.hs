module Lib
    ( main
    ) where

import ClassyPrelude
import Crypto.Random.Types (MonadRandom, getRandomBytes)

import qualified Platform.PG as PG
import qualified Platform.JWT as JWT
import qualified Platform.HTTP as HTTP

import qualified Feature.Auth.HTTP as AuthHTTP
import qualified Feature.Auth.JWT as AuthJWT

import qualified Feature.Version.HTTP as VersionHTTP
import qualified Feature.Version.Service as VersionService

import qualified Feature.User.HTTP as UserHTTP
import qualified Feature.User.Service as UserService
import qualified Feature.User.PG as UserPG
import qualified Feature.User.JWT as UserJWT

import qualified Feature.Item.HTTP as ItemHTTP
import qualified Feature.Item.Service as ItemService
import qualified Feature.Item.PG as ItemPG

main :: IO ()
main = do
    -- acquire resources
    pgEnv <- PG.init
    jwtEnv <- JWT.init

    -- start the application
    let runner app = flip runReaderT (pgEnv, jwtEnv) $ unAppT app
    HTTP.main runner

type Env = (PG.Env, JWT.Env)

newtype AppT a = AppT
    { unAppT :: ReaderT Env IO a
    } deriving ( Applicative, Functor, Monad
               , MonadIO, MonadReader Env)

instance MonadRandom AppT where
    getRandomBytes = liftIO . getRandomBytes

instance AuthHTTP.Service AppT where
    resolveToken = AuthJWT.resolveToken

instance VersionHTTP.Service AppT where
    getVersion = VersionService.getVersion

instance ItemHTTP.Service AppT where
    listItems = ItemService.listItems

instance UserHTTP.Service AppT where
    login = UserService.login
    register = UserService.register
    getUser = UserService.getUser
    updateUser = UserService.updateUser
    getProfile = UserService.getProfile
    followUser = UserService.followUser
    unfollowUser = UserService.unfollowUser

instance UserService.UserRepo AppT where
    findUserByAuth = UserPG.findUserByAuth
    findUserById = UserPG.findUserById
    addUser = UserPG.addUser
    updateUserById = UserPG.updateUserById

instance UserService.ProfileRepo AppT where
    findProfile = UserPG.findProfile
    followUserByUsername = UserPG.followUserByUsername
    unfollowUserByUsername = UserPG.unfollowUserByUsername

instance UserService.TokenRepo AppT where
    generateToken = UserJWT.generateToken

instance ItemService.ItemRepo AppT where
    listItemsByUser = ItemPG.listItemsByUser
