module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, Error, message)
import Control.Monad.Except (runExcept)
import Data.Array (concat)
import Data.Either (Either(..))
import Data.Foreign.Generic (decodeJSON)
import Data.Function.Uncurried (Fn3)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Global.Unsafe (unsafeStringify)
import Node.Buffer (BUFFER)
import Node.Encoding (Encoding(..))
import Node.Express.App (App, get, listenHttp, post, setProp, useExternal, useOnError)
import Node.Express.Handler (Handler)
import Node.Express.Middleware.Static (static)
import Node.Express.Request (getBody)
import Node.Express.Response (send, sendFile, sendJson, setStatus)
import Node.Express.Types (EXPRESS, Request, Response, ExpressM)
import Node.FS (FS)
import Node.FS.Sync as S
import Node.HTTP (Server)
import Node.Path as Path
import Node.Process (PROCESS, lookupEnv)
import Types (Notes(..), E)

foreign import jsonBodyParser :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)

notesPath :: String
notesPath = Path.concat ["src", "notes.json"]

parseInt :: String -> Int
parseInt str = fromMaybe 0 $ fromString str

notesGetHandler ::  forall e. Handler (
  fs :: FS,
  exception :: EXCEPTION,
  buffer :: BUFFER | e)
notesGetHandler = do
  sendFile notesPath

mainPageHandler :: forall e. Handler (
  fs :: FS,
  exception :: EXCEPTION,
  buffer :: BUFFER | e)
mainPageHandler = do
  static "dist"

notesPostHandler :: forall e. Handler (
  fs :: FS,
  exception :: EXCEPTION,
  buffer :: BUFFER | e)
notesPostHandler = do
  body <- getBody
  case (body :: E Notes) of
    Left reqError  -> send reqError
    Right (Notes reqNotes) -> do
      file <- liftEff $ S.readTextFile UTF8 notesPath
      case (runExcept (decodeJSON file) :: E Notes) of
        Left fileError -> send fileError
        Right (Notes fileNotes) -> do
          let newNotes = Notes { notes: concat [reqNotes.notes, fileNotes.notes] }
          _ <- liftEff $ S.writeTextFile UTF8 notesPath (unsafeStringify newNotes)
          send (unsafeStringify newNotes)

errorHandler :: forall e. Error -> Handler e
errorHandler err = do
  setStatus 400
  sendJson { error: message err }

appSetup :: forall e. App (
  fs :: FS,
  exception :: EXCEPTION,
  buffer :: BUFFER,
  console :: CONSOLE | e)
appSetup = do
  useExternal jsonBodyParser
  liftEff $ log "Setting up"
  setProp "json spaces" 4.0
  get "/api/notes"  notesGetHandler
  post "/api/notes" notesPostHandler
  get "*"           mainPageHandler
  useOnError        errorHandler

main :: forall e. Eff (
  fs :: FS,
  exception :: EXCEPTION,
  buffer :: BUFFER,
  express :: EXPRESS,
  process :: PROCESS,
  console :: CONSOLE | e
) Server
main = do
  port <- (parseInt <<< fromMaybe "8080") <$> lookupEnv "PORT"
  listenHttp appSetup port \_ ->
    log $ "Listening on " <> show port
