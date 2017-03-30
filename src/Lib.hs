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

-------------------
-- Configuration --
-------------------

-- | This 'Config' object is used to store environment about our application.
-- It is created on startup and passed to all the handlers.
data Config = Config
  { configPool :: !ConnectionPool  -- ^ A pool of database connections.
  , configPort :: !Port            -- ^ 'Port' to listen on.
  }

-- | Number of simultaneous database connections to use in the
-- 'ConnectionPool'.
type DbPoolConnNum = Int

-- | Create a 'ConnectionPool' for database connections based on a
-- 'ConnectionString'.
makePoolFromUrl
  :: DbPoolConnNum      -- ^ Number of database connections to use.
  -> ConnectionString
  -> IO ConnectionPool
makePoolFromUrl dbConnNum connectionString =
  runStdoutLoggingT $ createPostgresqlPool connectionString dbConnNum

-- | Create a 'Config' based on environment variables, using defaults if the
-- environment variables don't exist.
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

------------------------------------
-- App-specific Monad and Actions --
------------------------------------

-- | This 'MyApiM' monad is the monad that will be used for running the web
-- application handlers.  This includes 'helloWorldHandler',
-- 'addCommentHandler', and 'getCommentsHandler'.
--
-- It is just a newtype around @'ReaderT' 'Config' ('ExceptT' 'ServantErr' 'IO')@.
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

-- | The 'MonadBaseControl' instance for 'MyApiM' is required for using
-- 'runSqlPool' in 'runDb'.  It is somewhat complicated and can safely be
-- ignored if you've never seen it before.
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

---------------------
-- Web Application --
---------------------

-- | This 'API' type represents the three different routes for our web
-- application.
type API = "hello-world" :> Get '[JSON] Text
      :<|> "add-comment" :> ReqBody '[JSON] Comment :> Post '[JSON] Comment
      :<|> "get-comments" :> Get '[JSON] [Comment]


-- | Create a WAI 'Application' capable of running with Warp.
app :: Config -> Application
app config = serve (Proxy :: Proxy API) apiServer
  where
    apiServer :: Server API
    apiServer = enter naturalTrans serverRoot

    naturalTrans :: MyApiM :~> Handler
    naturalTrans = NT transformation

    -- This represents a natural transformation from 'MyApiM' to 'Handler'.
    -- This consists of unwrapping the 'MyApiM', running the
    -- @'ReaderT' 'Config'@, and wrapping the resulting value back up in a
    -- 'Handler'.
    transformation :: forall a . MyApiM a -> Handler a
    transformation = Handler . flip runReaderT config . unMyApiM

-- | Root of the web appliation.  This combines 'helloWorldHandler',
-- 'addCommentHandler', and 'getCommentsHandler'.
serverRoot :: ServerT API MyApiM
serverRoot = helloWorldHandler :<|> addCommentHandler :<|> getCommentsHandler

-- | Hello world handler.  Just returns the text @\"hello world\"@.
helloWorldHandler :: MyApiM Text
helloWorldHandler = pure "hello world"

-- | Adds a 'Comment' to the database.  Returns the 'Comment' that was added.
addCommentHandler :: Comment -> MyApiM Comment
addCommentHandler comment = do
  runDb $ insert_ comment
  pure comment

-- | Returns all the 'Comment's from the database.
getCommentsHandler :: MyApiM [Comment]
getCommentsHandler = do
  commentEntities <- runDb $ selectList [] []
  let comments = fmap entityVal commentEntities
  pure comments

----------
-- Main --
----------

defaultMain :: IO ()
defaultMain = do
  config <- createConfigFromEnvVars
  runSqlPool (runMigration migrateAll) $ configPool config
  putStrLn $
    "running servant-on-heroku on port " <> show (configPort config) <> "..."
  run (configPort config) . logStdoutDev $ app config

