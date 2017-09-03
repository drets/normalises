module Component where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Except (runExcept)
import Data.Array (filter, index, length, range, reverse, zip)
import Data.Either (Either(..))
import Data.Foreign.Generic (decodeJSON)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Tuple (Tuple(..))
import EditField as EditField
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
             | HandlePropertyEdit EditField.Message a
             | HandleValueEdit EditField.Message a

type State = {
  records  :: Maybe (Array Note),
  property :: Maybe String,
  value    :: Maybe String
}

newtype Slot = EditFieldSlot Int

derive newtype instance eqSlot :: Eq Slot
derive newtype instance ordSlot :: Ord Slot

type AppEffects eff =
  Aff
  ( ajax :: AX.AJAX
  , console :: CONSOLE
  | eff )

component :: forall eff. H.Component HH.HTML Query Unit Void (AppEffects eff)
component =
  H.lifecycleParentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just (H.action GetNotes)
    , finalizer: Nothing
    }
  where

  initialState :: State
  initialState = {
    records: Nothing,
    property: Nothing,
    value: Nothing
  }

  render :: State -> H.ParentHTML Query EditField.Query Slot (AppEffects eff)
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
            Just arr -> showRow <$> zip (reverse arr) (range 1 (length arr))

      ]

  showRow (Tuple note@(Note n) i) = HH.div
                   [ HP.class_ (ClassName "note") ]
                   -- small hacks: need some unique ids; had to pass editing equals false.
                   [ HH.slot (EditFieldSlot i)
                             EditField.component
                             { value: n.property, id: n.id, editing: false }
                             (HE.input HandlePropertyEdit)
                   , HH.slot (EditFieldSlot (-i))
                             EditField.component
                             { value: n.value, id: n.id, editing: false }
                             (HE.input HandleValueEdit)
                   , (deleteButton note)
                   ]

  deleteButton (Note note) = HH.button
                               [ HE.onClick (HE.input_ (DeleteNote note.id))]
                               [ HH.text "Delete"]

  findNote :: Maybe (Array Note) -> Int -> Maybe Note
  findNote Nothing _    = Nothing
  findNote (Just xs) id = index filtered 0
    where
      filtered :: Array Note
      filtered = filter (\(Note note) -> note.id == id) xs

  eval :: Query ~> H.ParentDSL State Query EditField.Query Slot Void (AppEffects eff)
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

    HandleValueEdit (EditField.Saved editState) next -> do
      state <- H.get
      case findNote state.records editState.id of
        Just (Note sRecord) -> do
          let record = Note { property: sRecord.property, value: editState.value, id: editState.id }
          _ <- H.liftAff $ AX.post_ ("/api/note/" <> (show editState.id)) record
          eval (GetNotes next)
        Nothing -> pure next

    HandlePropertyEdit (EditField.Saved editState) next -> do
      -- code duplication
      state <- H.get
      case findNote state.records editState.id of
        Just (Note sRecord) -> do
          let record = Note { property: editState.value, value: sRecord.value, id: editState.id }
          _ <- H.liftAff $ AX.post_ ("/api/note/" <> (show editState.id)) record
          eval (GetNotes next)
        Nothing -> pure next
