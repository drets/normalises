module EditField where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Query a = Edit a
             | Handle State a
             | UpdateField String a
             | SaveField a

type State = {
  value   :: String,
  editing :: Boolean,
  id      :: Int
}

type Input = State

data Message = Saved State

component :: forall m. H.Component HH.HTML Query Input Message m
component =
  H.component
    { initialState: id
    , render
    , eval
    , receiver: HE.input Handle
    }
  where

  render :: State -> H.ComponentHTML Query
  render state =
    if state.editing
      then
        HH.input
          [ HP.type_ HP.InputText
          , HP.placeholder "Value"
          , HP.value state.value
          , HE.onValueChange (HE.input UpdateField)
          , HE.onBlur (HE.input (const SaveField))
          ]
     else
        HH.p
          [ HE.onClick (HE.input_ Edit) ]
          [ HH.text state.value ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Edit next -> do
      H.modify (\state -> state { editing = not state.editing })
      pure next

    Handle n next -> do
      H.modify (\state -> state { value = n.value })
      pure next

    UpdateField value next -> do
      H.modify (\state -> state { value = value })
      pure next

    SaveField next -> do
      state <- H.get
      let nextState = state { editing = false }
      H.put nextState
      H.raise $ Saved state
      pure next
