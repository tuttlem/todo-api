module Lib
    ( main
    ) where

import ClassyPrelude
import Crypto.Random.Types (MonadRandom, getRandomBytes)

import qualified Platform.HTTP as HTTP

main :: IO ()
main = do
    -- acquire resources

    -- start the application
    let runner app = flip runReaderT () $ unAppT app
    HTTP.main runner

type Env = ()

newtype AppT a = AppT
    { unAppT :: ReaderT Env IO a
    } deriving ( Applicative, Functor, Monad
               , MonadIO, MonadReader Env)

instance MonadRandom AppT where
    getRandomBytes = liftIO . getRandomBytes
