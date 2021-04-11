import ClassyPrelude

import Test.Hspec
import System.Environment
import Database.PostgreSQL.Simple
import Control.Concurrent
import Spec.Common

import qualified Misc.Client as RW
import qualified Lib

import qualified Spec.Version as Version

main :: IO ()
main = withEnv . hspec $ do
  Version.spec

withEnv :: IO () -> IO ()
withEnv = bracket startEnv cleanEnv . const

startEnv :: IO ThreadId
startEnv = do
  execPGQuery ["drop database if exists todo_test", "create database todo_test"]
  setEnv "DATABASE_URL" "postgresql://postgres:password@127.0.0.1/todo_test"
  setEnv "ENABLE_HTTPS" "False"
  setEnv "JWT_EXPIRATION_SECS" "8"

  tId <- forkIO Lib.main
  unlessM healthCheck $ do
    putStrLn "Waiting for server ..."
    threadDelay 1000000
  return tId
  where
    healthCheck =
      either (const False) (const True) <$> runClient RW.version

cleanEnv :: ThreadId -> IO ()
cleanEnv tId = do
  killThread tId
  putStrLn $ "Server Killed (" <> tshow tId <> ")"

execPGQuery :: [Query] -> IO ()
execPGQuery qrys =
  bracket acquire release execQuery
    where
      acquire = connectPostgreSQL "postgresql://postgres:password@127.0.0.1"
      release = close
      execQuery conn = forM_ qrys (void . execute_ conn)

