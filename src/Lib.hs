{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Natural ((:~>)(NT))
import Database.Persist.Postgresql
       (ConnectionString, createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool)
import Data.Proxy (Proxy(Proxy))
import Network.Wai.Handler.Warp (Port, run)
import Network.Wai.Middleware.RequestLogger
       (logStdoutDev, logStdout)
import Network.Wai (Application, Middleware)
import Servant
       (Get, Handler(Handler), JSON, Post, ServantErr, Server, ServerT,
        (:>), (:<|>)((:<|>)), enter, serve)
import System.ReadEnvVar (lookupEnvDef, readEnvVarDef)

data Config = Config
  { configPool :: !ConnectionPool
  , configPort :: !Port
  }

type DbPoolConnNum = Int

makePoolFromUrl
  :: DbPoolConnNum
  -> ConnectionString
  -> IO ConnectionPool
makePoolFromUrl dbConnNum connectionString =
  runStdoutLoggingT $ createPostgresqlPool connectionString dbConnNum

createConfigFromEnvVars :: IO Config
createConfigFromEnvVars = do
  port <- readEnvVarDef "PORT" 8080
  dbConnNum <- readEnvVarDef "DATABASE_CONNECTION_NUM" 10
  dbConnectionString <-
    lookupEnvDef
      "DATABASE_URL"
      "postgres://mydbuser:mydbpass@localhost:5432/mydb"
  pool <- makePoolFromUrl dbConnNum dbConnectionString
  pure Config {configPool = pool, configPort = port}

type API = "add-comment" :> Post '[JSON] String
      :<|> "get-comments" :> Get '[JSON] [String]

newtype MyApiM a = MyApiM
  { unMyApiM :: ReaderT Config (ExceptT ServantErr IO) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError ServantErr
             , MonadIO
             , MonadReader Config
             )

app :: Config -> Application
app config = serve (Proxy :: Proxy API) apiServer
  where
    apiServer :: Server API
    apiServer = enter naturalTrans serverRoot

    naturalTrans :: MyApiM :~> Handler
    naturalTrans = NT transformation

    transformation :: forall a . MyApiM a -> Handler a
    transformation = Handler . flip runReaderT config . unMyApiM

serverRoot :: ServerT API MyApiM
serverRoot = addComments :<|> getComments

addComments :: MyApiM String
addComments = return "hello"

getComments :: MyApiM [String]
getComments = return ["hello"]

defaultMain :: IO ()
defaultMain = do
  config <- createConfigFromEnvVars
  run (configPort config) $ app config

