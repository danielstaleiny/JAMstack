module Main where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Debug.Trace as D
import Effect (Effect)
import Effect.Aff (Aff, delay, forkAff, joinFiber, launchAff_, launchAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Web.DOM.DOMTokenList (add, remove)
import Web.DOM.Element (Element, classList, className, setClassName, toNode)
import Web.DOM.Node (textContent)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

-- import Web.DOM.Document (createElement)

-- >>= bind
-- <$> map or fmap from haskell
-- >>> pipe
-- String <- Effect String
-- name <- value
-- M a | a = String, Maybe etc.
-- let name = value


-- getJson = unit

-- post = unit

-- get = unit

fadeIn :: Element -> Effect Unit
fadeIn elem = do
  classList <- classList elem
  remove classList "opacity-0"
  add classList "transition-opacity"
  add classList "duration-300"

fadeIn_ :: Maybe Element -> Effect Unit
fadeIn_ Nothing = Console.log "couldn't find it"
fadeIn_ (Just elem) = fadeIn elem

-- expect to have opacity-0 on element
-- alternatively add opacity-100 instead
-- test if you have opacity-0 and opacity-100 what has preccedance.
-- el.classList.add('transition-opacity');
-- el.classList.add('duration-300');
-- el.classList.remove('opacity-0');
-- resolve Nothing = pure "ok"
-- resolve _ = pure "ok"

main :: Effect Unit
main = do
  doc <- window >>= document
  elem_ <- toNonElementParentNode >>> getElementById "hide-elem" $ doc -- Maybe elem
  fadeIn_ elem_
  -- fadeIn_ elem_ -- Maybe Effect unit


  -- elem1 <- fadeIn <$> elem
  -- fromMaybe "ok" <$> elem1 >>> pure
  -- Console.log


getById :: String -> String -> Effect String
getById fallback id = window
       >>= document
       >>= toNonElementParentNode >>> pure
       >>= getElementById id
       >>= map toNode
       >>> map textContent
       >>> fromMaybe (pure fallback)


-- slowInt :: Int -> Aff Int
-- slowInt int = do
--   delay $ Milliseconds 1000.0
--   pure int

-- slowAdd :: Int -> Int -> Aff Int
-- slowAdd a b = do
--   fiberA <- forkAff $ slowInt a
--   fiberB <- forkAff $ slowInt b

--   slowA <- joinFiber fiberA
--   slowB <- joinFiber fiberB

--   pure $ slowA + slowB



-- main :: Effect Unit
-- main = launchAff_ do
--        result <- slowAdd 1 2
--        liftEffect $ Console.logShow result


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
