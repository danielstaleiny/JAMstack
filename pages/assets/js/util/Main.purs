module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(..), fromMaybe)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toNonElementParentNode, toDocument)
--import Web.HTML.HTMLDocument (toParentNode, HTMLDocument)
import Web.DOM.Element (Element, toNode, setId)
import Web.DOM.NonElementParentNode (getElementById)
import Web.DOM.Node (textContent, setTextContent, appendChild)
import Web.DOM.Document (createElement)

getById :: String -> Effect String
getById id = window
       >>= document
       >>= toNonElementParentNode >>> pure
       >>= getElementById id
    -- bind fmap
       >>= (<$>) toNode
    -- pipe fmap
       >>> (<$>) textContent
    -- pipe
       >>> fromMaybe (pure "Coudn't find anything")


main :: Effect Unit
main = do
  getById "hide-elem" >>= log


  -- log d
