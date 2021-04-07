module Lib
    ( main
    ) where

import ClassyPrelude
import Crypto.Random.Types (MonadRandom, getRandomBytes)

import qualified Platform.PG as PG
import qualified Platform.JWT as JWT
import qualified Platform.HTTP as HTTP

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
