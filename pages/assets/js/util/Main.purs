module Main where

import Prelude
import Effect (Effect)
import Data.Time.Duration (Milliseconds(..))
import Effect.Console as Console
import Debug.Trace as D
-- Control.Monad.Eff.Console
import Effect.Class (liftEffect)
import Effect.Aff (Aff, delay, forkAff, joinFiber, launchAff_, launchAff)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe (fromMaybe)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.DOM.Element (toNode)
import Web.DOM.NonElementParentNode (getElementById)
-- import Web.DOM.Node (textContent, setTextContent, appendChild)
import Web.DOM.Node (textContent)
-- import Web.DOM.Document (createElement)

-- >>= bind
-- <$> map or fmap from haskell
-- >>> pipe
-- String <- Effect String
-- name <- value
-- M a | a = String, Maybe etc.
-- let name = value


-- getById :: String -> String -> Effect String
-- getById fallback id = window
--        >>= document
--        >>= toNonElementParentNode >>> pure
--        >>= getElementById id
--        >>= map toNode
--        >>> map textContent
--        >>> fromMaybe (pure fallback)


slowInt :: Int -> Aff Int
slowInt int = do
  delay $ Milliseconds 1000.0
  pure int

slowAdd :: Int -> Int -> Aff Int
slowAdd a b = do
  fiberA <- forkAff $ slowInt a
  fiberB <- forkAff $ slowInt b

  slowA <- joinFiber fiberA
  slowB <- joinFiber fiberB

  pure $ slowA + slowB



main :: Effect Unit
main = launchAff_ do
       result <- slowAdd 1 2
       liftEffect $ Console.logShow result


-- forall a. Show a => a -> Effect Unit
-- logShow
--
-- String -> Effect Unit
-- log

-- forall a m. MonadEffect m => Effect a -> m a
-- liftEffect
--
-- Milliseconds -> Aff Unit
-- delay
--
-- forall a. Aff a -> Aff (Fiber a)
-- forkAff
-- forall a. Fiber a -> Aff a
-- joinFiber
-- forall a. Aff a -> Effect Unit
-- launchAff_
-- forall a. Aff a -> Effect (Fiber a)
-- launchAff
--
--


-- main :: Effect Unit
-- main = D.trace "hello" \_ -> Console.log "there"
-- main = D.traceM $ "hello3"
-- main = D.spy "hello" $ Console.log "there"


-- main :: Effect Unit
-- main = getById "Couldn't find anything" "hide-elem" >>= Console.log
