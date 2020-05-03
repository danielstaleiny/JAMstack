module Main where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Debug.Trace (traceM, trace) as D
import Effect (Effect)
import Effect.Aff (Aff, delay, forkAff, joinFiber, launchAff_, launchAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Web.DOM.Element (Element)
import Web.DOM.DOMTokenList (add, remove, toggle) as DOM
import Web.DOM.Element (classList, className, setClassName, toNode) as DOM
import Web.DOM.Node (textContent) as DOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (HTMLElement)
import Web.HTML (window) as HTML
import Web.HTML.HTMLDocument (toNonElementParentNode) as HTML
import Web.HTML.HTMLElement (fromElement, hidden, setHidden) as HTML
import Web.HTML.Window (document) as HTML
import Web.Event.EventTarget as Evt

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
--
-- void :: forall f a. Functor f => f a -> f Unit
--
fadeToggle :: Element -> Effect Unit
fadeToggle elem = do
  classList <- DOM.classList elem
  bool <- DOM.toggle classList "opacity-0"
  -- void $ pure bool
  Console.logShow bool


-- toggle element add or remove element and return bool value if it added or remove it.
-- remove false
-- added true
fadeToggle_ :: Maybe Element -> Effect Unit
fadeToggle_ Nothing = Console.log "couldn't find it"
fadeToggle_ (Just elem) = fadeToggle elem


fadeIn :: Element -> Effect Unit
fadeIn elem = do
  classList <- DOM.classList elem
  DOM.remove classList "opacity-0"

fadeIn_ :: Maybe Element -> Effect Unit
fadeIn_ Nothing = Console.log "couldn't find it"
fadeIn_ (Just elem) = fadeIn elem


fadeOut :: Element -> Effect Unit
fadeOut elem = do
  classList <- DOM.classList elem
  DOM.add classList "opacity-0"

fadeOut_ :: Maybe Element -> Effect Unit
fadeOut_ Nothing = Console.log "couldn't find it"
fadeOut_ (Just elem) = fadeOut elem


toggleHidden :: Element -> Effect Unit
toggleHidden elem = do
  let htmlele_ = HTML.fromElement elem
  case htmlele_ of (Nothing) -> Console.log "Didn't find element"
                   (Just htmlele) -> do
                     b <- HTML.hidden htmlele
                     a <- case b of true -> HTML.setHidden false htmlele
                                    false -> HTML.setHidden true htmlele
                     pure a

toggleHidden_ :: Maybe Element -> Effect Unit
toggleHidden_ Nothing = Console.log "Didn't find element"
toggleHidden_ (Just elem) = toggleHidden elem


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
  doc <- HTML.window >>= HTML.document
  elem_ <- HTML.toNonElementParentNode >>> DOM.getElementById "hide-elem" $ doc -- Maybe elem
  fadeToggle_ elem_
  toggleHidden_ elem_
  -- fadeIn_ elem_ -- Maybe Effect unit


  -- elem1 <- fadeIn <$> elem
  -- fromMaybe "ok" <$> elem1 >>> pure
  -- Console.log


getById :: String -> String -> Effect String
getById fallback id = HTML.window
       >>= HTML.document
       >>= HTML.toNonElementParentNode >>> pure
       >>= DOM.getElementById id
       >>= map DOM.toNode
       >>> map DOM.textContent
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
