module Main where

import Prelude

import Control.Monad.Aff (Canceler, launchAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, Error, error, message)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Database.Postgres (Client, DB, Query(..), connect, execute, query_)
import Database.Postgres.SqlValue (toSql)
import Node.Buffer (BUFFER)
import Node.Express.App (App, delete, get, listenHttp, post, setProp, useExternal, useOnError)
import Node.Express.Handler (Handler, nextThrow)
import Node.Express.Middleware.Static (static)
import Node.Express.Request (getBody, getRouteParam)
import Node.Express.Response (send, sendJson, setStatus)
import Node.Express.Types (EXPRESS, Request, Response, ExpressM)
import Node.FS (FS)
import Node.Process (PROCESS, lookupEnv)
import Types (Note(..), E)

foreign import jsonBodyParser :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

parseInt :: String -> Int
parseInt str = fromMaybe 0 $ fromString str

selectQuery :: String
selectQuery = "SELECT id, property, value, created FROM notes;"

insertQuery :: String
insertQuery = "INSERT INTO notes (property, value, created) VALUES ($1, $2, current_timestamp);"

deleteQuery :: String
deleteQuery = "DELETE from notes where id=$1;"

notesGetHandler :: forall e. Client -> Handler (db :: DB | e)
notesGetHandler client = do
  rows <- liftAff $ query_ (Query selectQuery :: Query Note) client
  sendJson rows

mainPageHandler :: forall e. Handler (
  fs :: FS,
  exception :: EXCEPTION,
  buffer :: BUFFER | e)
mainPageHandler = do
  static "dist"

notePostHandler :: forall e. Client -> Handler (db :: DB | e)
notePostHandler client = do
  body <- getBody
  case (body :: E Note) of
    Left reqError  -> send reqError
    Right (Note reqNotes) -> do
      _ <- liftAff $ execute (Query insertQuery) [toSql reqNotes.property, toSql reqNotes.value] client
      send "done"

noteDeleteHandler :: forall e. Client -> Handler (db :: DB | e)
noteDeleteHandler client = do
  idParam <- getRouteParam "id"
  case idParam of
    Nothing -> nextThrow $ error "id is required"
    Just id -> do
      _ <- liftAff $ execute (Query deleteQuery) [toSql id] client
      send "done"

errorHandler :: forall e. Error -> Handler e
errorHandler err = do
  setStatus 400
  sendJson { erfror: message err }

appSetup :: forall e. Client -> App (
  fs :: FS,
  exception :: EXCEPTION,
  buffer :: BUFFER,
  db :: DB,
  console :: CONSOLE | e)
appSetup client = do
  useExternal jsonBodyParser
  liftEff $ log "Setting up"
  setProp "json spaces" 4.0
  get "/api/notes"       (notesGetHandler client)
  post "/api/note"       (notePostHandler client)
  delete "/api/note/:id" (noteDeleteHandler client)
  get "*"                mainPageHandler
  useOnError             errorHandler

type AppEffects eff =
  ( fs :: FS
  ,  exception :: EXCEPTION
  ,  buffer :: BUFFER
  ,  express :: EXPRESS
  ,  process :: PROCESS
  ,  db :: DB
  ,  console :: CONSOLE
  | eff )

connectionInfo :: { host :: String, db :: String, port :: Int, user :: String, password :: String }
connectionInfo =
  { host: "localhost"
  , db: "test"
  , port: 5432
  , user: "testuser"
  , password: "test"
  }

main :: forall eff. Eff (AppEffects (exception :: EXCEPTION | eff))
   (Canceler (AppEffects eff))
main = launchAff $ do
  client <- connect connectionInfo
  port <- liftEff $ (parseInt <<< fromMaybe "8080") <$> lookupEnv "PORT"
  liftEff $ listenHttp (appSetup client) 8080 \_ ->
    log $ "Listening on " <> show 8080
