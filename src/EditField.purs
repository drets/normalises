module EditField where

import Prelude

import CSS as CSS
import CSS.Common as CSS.Common
import Control.Monad.Aff (Aff)
import DOM (DOM)
import DOM.Event.KeyboardEvent (key)
import DOM.HTML.HTMLElement (focus)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style) as HP.CSS
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

component :: forall e. H.Component HH.HTML Query Input Message (Aff ( dom :: DOM | e ))
component =
  H.component
    { initialState: _ { editing = false }
    , render
    , eval
    , receiver: HE.input Handle
    }
  where

  displayWhen = when <<< not <@> CSS.display CSS.displayNone
  onEnter q = HE.onKeyPress \e ->
    if key e == "Enter"
      then Just (q unit)
      else Nothing

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.input
          [ HP.type_ HP.InputText
          , HP.placeholder "Value"
          , HP.value state.value
          , HP.ref (H.RefLabel "input")
          , HE.onValueChange (HE.input UpdateField)
          , HE.onBlur (HE.input_ SaveField)
          , HE.onFocus (HE.input_ Edit)
          , onEnter SaveField
          , HP.CSS.style do
              displayWhen state.editing
              CSS.key (CSS.fromString "font") (CSS.Common.inherit :: CSS.FontWeight {- hack -})
          ]
      , HH.p
          [ HE.onClick (HE.input_ Edit)
          , HP.CSS.style (displayWhen (not state.editing))
          ]
          [ HH.text state.value ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Message (Aff ( dom :: DOM | e ))
  eval = case _ of
    Edit next -> next <$ do
      H.modify (\state -> state { editing = true })
      H.getHTMLElementRef (H.RefLabel "input") >>= traverse_
        \el -> H.liftEff $ focus el

    Handle n next -> next <$ do
      H.modify _ { value = n.value }

    UpdateField value next -> next <$ do
      H.modify _ { value = value }

    SaveField next -> next <$ do
      state <- H.get
      let nextState = state { editing = false }
      H.put nextState
      H.raise $ Saved nextState
