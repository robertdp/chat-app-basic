module Client where

import Prelude

import Client.App as App
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM as DOM
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window as Window


main :: Effect Unit
main = do
  document <- Window.document =<< window
  container <- getElementById "app" $ toNonElementParentNode document
  case container of
    Nothing -> throw "Container element not found."
    Just c  -> DOM.render App.app c