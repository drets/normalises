module FrontEnd where

import Prelude

import Component (component)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax as AX

main :: Eff (HA.HalogenEffects (ajax :: AX.AJAX, console :: CONSOLE)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
