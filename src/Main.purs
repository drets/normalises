module Main where

import Prelude

import Control.Monad.Aff (Aff, Canceler, launchAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, Error, message)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Node.Buffer (BUFFER)
import Node.Express.App (App, get, listenHttp, post, setProp, useExternal, useOnError)
import Node.Express.Handler (Handler)
import Node.Express.Middleware.Static (static)
import Node.Express.Request (getBody)
import Node.Express.Response (send, sendJson, setStatus)
import Node.Express.Types (EXPRESS, Request, Response, ExpressM)
import Node.FS (FS)
import Node.Process (PROCESS, lookupEnv)
import SQLite3 (DBConnection, DBEffects, FilePath, newDB, queryDB)
import Types (Note(..), E)

foreign import jsonBodyParser :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

parseInt :: String -> Int
parseInt str = fromMaybe 0 $ fromString str

selectQuery :: String
selectQuery = "SELECT property, value, created FROM notes;"

insertQuery :: String
insertQuery = "INSERT OR REPLACE INTO notes (property, value, created) VALUES ($1, $2, datetime());"

createQuery :: String
createQuery = "CREATE TABLE IF NOT EXISTS notes (property varchar(20) primary key unique, value varchar(20), created datetime);"

notesGetHandler :: forall e. DBConnection -> Handler (db :: DBEffects | e)
notesGetHandler db = do
  let queryDB' query params = queryDB db query params
  rows <- liftAff $ queryDB' selectQuery []
  sendJson rows

mainPageHandler :: forall e. Handler (
  fs :: FS,
  exception :: EXCEPTION,
  buffer :: BUFFER | e)
mainPageHandler = do
  static "dist"

notePostHandler :: forall e. DBConnection -> Handler (db :: DBEffects | e)
notePostHandler db = do
  let queryDB' query params = queryDB db query params
  body <- getBody
  case (body :: E Note) of
    Left reqError  -> send reqError
    Right (Note reqNotes) -> do
      _ <- liftAff $ queryDB' insertQuery [reqNotes.property, reqNotes.value]
      send "done"

errorHandler :: forall e. Error -> Handler e
errorHandler err = do
  setStatus 400
  sendJson { error: message err }

appSetup :: forall e. DBConnection -> App (
  fs :: FS,
  exception :: EXCEPTION,
  buffer :: BUFFER,
  db :: DBEffects,
  console :: CONSOLE | e)
appSetup db = do
  useExternal jsonBodyParser
  liftEff $ log "Setting up"
  setProp "json spaces" 4.0
  get "/api/notes"  (notesGetHandler db)
  post "/api/note"  (notePostHandler db)
  get "*"           mainPageHandler
  useOnError        errorHandler

ensureDB :: forall eff. FilePath -> Aff (db :: DBEffects | eff) DBConnection
ensureDB path = do
  db <- newDB path
  _ <- queryDB db createQuery []
  pure db

type AppEffects eff =
  ( fs :: FS
  ,  exception :: EXCEPTION
  ,  buffer :: BUFFER
  ,  express :: EXPRESS
  ,  process :: PROCESS
  ,  db :: DBEffects
  ,  console :: CONSOLE
  | eff )

main :: forall eff. Eff (AppEffects (exception :: EXCEPTION | eff))
   (Canceler (AppEffects eff))
main = launchAff $ do
  db <- (ensureDB "notes")
  port <- liftEff $ (parseInt <<< fromMaybe "8080") <$> lookupEnv "PORT"
  liftEff $ listenHttp (appSetup db) 8080 \_ ->
    log $ "Listening on " <> show 8080
