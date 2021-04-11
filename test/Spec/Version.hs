module Spec.Version (spec) where

import ClassyPrelude
import Test.Hspec
import Feature.Common.Types
import Feature.Version.Types
import qualified Misc.Client as RW
import Text.StringRandom
import Control.Concurrent

import Spec.Common

spec :: Spec
spec = do
  describe "version" $ do

    describe "retrieve" $ do

      it "should retrieve successfully" $ do
        Right info <- runClient $ RW.version
        versionInfoName info `shouldBe` "todo-api-test"

