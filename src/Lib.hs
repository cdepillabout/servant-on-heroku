{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Control.Monad.Base (MonadBase)
import Control.Monad.Except (ExceptT(ExceptT), MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Reader
       (MonadReader, ReaderT(ReaderT), reader, runReaderT)
import Control.Monad.Trans.Control
       (MonadBaseControl(StM, liftBaseWith, restoreM))
import Control.Natural ((:~>)(NT))
import Data.Monoid ((<>))
import Data.Text (Text)
import Database.Persist (Entity(entityVal), insert_, selectList)
import Database.Persist.Postgresql
       (ConnectionString, createPostgresqlPool)
import Database.Persist.Sql
       (ConnectionPool, SqlBackend, runMigration, runSqlPool)
import Data.Proxy (Proxy(Proxy))
import Network.Wai.Handler.Warp (Port, run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai (Application)
import Servant
       (Get, Handler(Handler), JSON, Post, ReqBody, ServantErr, Server, ServerT,
        (:>), (:<|>)((:<|>)), enter, serve)
import System.ReadEnvVar (lookupEnvDef, readEnvVarDef)

import Models (Comment, migrateAll)

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

type API = "hello-world" :> Get '[JSON] Text
      :<|> "add-comment" :> ReqBody '[JSON] Comment :> Post '[JSON] Comment
      :<|> "get-comments" :> Get '[JSON] [Comment]

newtype MyApiM a = MyApiM
  { unMyApiM :: ReaderT Config (ExceptT ServantErr IO) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadBase IO
             , MonadError ServantErr
             , MonadIO
             , MonadReader Config
             )

instance MonadBaseControl IO MyApiM where
  type StM MyApiM a = Either ServantErr a

  liftBaseWith
    :: forall a.
       ((forall x. MyApiM x -> IO (Either ServantErr x)) -> IO a) -> MyApiM a
  liftBaseWith f =
    MyApiM $
    ReaderT $ \r ->
      ExceptT $
      fmap Right $
      f $ \(MyApiM readerTExceptT) -> runExceptT $ runReaderT readerTExceptT r

  restoreM :: forall a. Either ServantErr a -> MyApiM a
  restoreM eitherA = MyApiM . ReaderT . const . ExceptT $ pure eitherA

-- | Run a Persistent query.
runDb :: ReaderT SqlBackend MyApiM a -> MyApiM a
runDb query = do
  pool <- reader configPool
  runSqlPool query pool

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
serverRoot = helloWorldHandler :<|> addCommentHandler :<|> getCommentsHandler

helloWorldHandler :: MyApiM Text
helloWorldHandler = pure "hello world"

addCommentHandler :: Comment -> MyApiM Comment
addCommentHandler comment = do
  runDb $ insert_ comment
  pure comment

getCommentsHandler :: MyApiM [Comment]
getCommentsHandler = do
  commentEntities <- runDb $ selectList [] []
  let comments = fmap entityVal commentEntities
  pure comments

defaultMain :: IO ()
defaultMain = do
  config <- createConfigFromEnvVars
  runSqlPool (runMigration migrateAll) $ configPool config
  putStrLn $
    "running servant-on-heroku on port " <> show (configPort config) <> "..."
  run (configPort config) . logStdoutDev $ app config

