module Spec.Common where

import ClassyPrelude

import qualified Misc.Client as RW

runClient :: ReaderT RW.RWBaseUrl m a -> m a
runClient = flip runReaderT (RW.RWBaseUrl "http://127.0.0.1:3000")
