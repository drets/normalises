module Component where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign.Generic (decodeJSON)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Global.Unsafe (unsafeStringify)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Types (E, Note(..))

data Query a = GetNotes a
             | UpdateProperty String a
             | UpdateValue String a
             | SendNotes a
             | DeleteNote Int a

type State = { records :: Maybe (Array Note), property :: Maybe String, value :: Maybe String }

component :: forall eff. H.Component HH.HTML Query Unit Void (Aff (ajax :: AX.AJAX, console :: CONSOLE | eff))
component =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just (H.action GetNotes)
    , finalizer: Nothing
    }
  where

  initialState :: State
  initialState = { records: Nothing, property: Nothing, value: Nothing }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Notes:" ]
      , HH.div [ HP.class_ (ClassName "header") ]
        [ HH.input
            [ HP.type_ HP.InputText
            , HP.placeholder "Property"
            , HE.onValueChange (HE.input UpdateProperty)
            ]
        , HH.input
            [ HP.type_ HP.InputText
            , HP.placeholder "Value"
            , HE.onValueChange (HE.input UpdateValue)
            ]
        , HH.button
            [ HE.onClick (HE.input_ SendNotes) ]
            [ HH.text "Update"]
            ]
      , HH.div_
          case state.records of
            Nothing -> [ HH.p_ [ HH.text "[]"] ]
            Just arr -> (\(Note r) ->
                          HH.div [ HP.class_ (ClassName "note") ]
                                 [ HH.p_ [ HH.text r.property ],
                                   HH.p_ [ HH.text r.value ],
                                   HH.button
                                     [ HE.onClick (HE.input_ (DeleteNote r.id))]
                                     [ HH.text "Delete"]]) <$> arr
      ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (ajax :: AX.AJAX, console :: CONSOLE | eff))
  eval = case _ of
    GetNotes next -> do
      response <- H.liftAff $ AX.get "/api/notes"
      case (runExcept (decodeJSON response.response) :: E (Array Note)) of
        Right notes -> H.modify (\state -> state { records = Just notes })
        Left e -> liftEff (log $ unsafeStringify e)
      pure next
    UpdateProperty property next -> do
      H.modify (\state -> state { property = Just property })
      pure next
    UpdateValue value next -> do
      H.modify (\state -> state { value = Just value })
      pure next
    DeleteNote id next -> do
      _ <- H.liftAff $ AX.delete_ ("/api/note/" <> (show id))
      eval (GetNotes next)
    SendNotes next -> do
      state <- H.get
      if (isJust state.property && isJust state.value)
        then do
          -- small hack with 0 − it's ignored on server.
          let record = Note { property: fromMaybe "" state.property, value: fromMaybe "" state.value, id: 0 }
          _ <- H.liftAff $ AX.post_ "/api/note" record
          eval (GetNotes next)
        else
          pure next
