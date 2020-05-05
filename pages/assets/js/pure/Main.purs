module Main where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Unit (unit)
import Effect (Effect)
import Effect.Aff (Aff, delay, forkAff, joinFiber, launchAff_, launchAff)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Web.DOM.DOMTokenList (add, remove, toggle) as DOM
import Web.DOM.Element (Element, classList, className, setClassName, toNode, toEventTarget)
import Web.DOM.Node (textContent)
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (EventListener, addEventListener, eventListener)
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.HTMLElement (fromElement, hidden, setHidden)
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
--
-- void :: forall f a. Functor f => f a -> f Unit
--
fadeToggle :: Element -> Effect Unit
fadeToggle elem = do
  classList <- classList elem
  bool <- DOM.toggle classList "opacity-0"
  -- void $ pure bool
  logShow bool


-- toggle element add or remove element and return bool value if it added or remove it.
-- remove false
-- added true
fadeToggle_ :: Maybe Element -> Effect Unit
fadeToggle_ Nothing = log "couldn't find it"
fadeToggle_ (Just elem) = fadeToggle elem


fadeIn :: Element -> Effect Unit
fadeIn elem = do
  classList <- classList elem
  DOM.remove classList "opacity-0"

fadeIn_ :: Maybe Element -> Effect Unit
fadeIn_ Nothing = log "couldn't find it"
fadeIn_ (Just elem) = fadeIn elem


fadeOut :: Element -> Effect Unit
fadeOut elem = do
  classList <- classList elem
  DOM.add classList "opacity-0"

fadeOut_ :: Maybe Element -> Effect Unit
fadeOut_ Nothing = log "couldn't find it"
fadeOut_ (Just elem) = fadeOut elem


toggleHidden :: Element -> Effect Unit
toggleHidden elem = do
  let htmlele_ = fromElement elem
  case htmlele_ of (Nothing) -> log "Didn't find element"
                   (Just htmlele) -> do
                     b <- hidden htmlele
                     a <- case b of true -> setHidden false htmlele
                                    false -> setHidden true htmlele
                     pure a

toggleHidden_ :: Maybe Element -> Effect Unit
toggleHidden_ Nothing = log "Didn't find element"
toggleHidden_ (Just elem) = toggleHidden elem


addClickEvent :: EventListener -> Element -> Effect Unit
addClickEvent cb elem = do
  let et = toEventTarget elem
  addEventListener (EventType "click") cb false et


-- JS addEventListener
-- options = {passive: true}
-- false -- indicating thot useCapture is false, Add this for best compatibility.

-- expect to have opacity-0 on element
-- alternatively add opacity-100 instead
-- test if you have opacity-0 and opacity-100 what has preccedance.
-- el.classList.add('transition-opacity');
-- el.classList.add('duration-300');
-- el.classList.remove('opacity-0');
-- resolve Nothing = pure "ok"
-- resolve _ = pure "ok"
ignore :: forall m. Applicative m => m Unit
ignore = pure unit



main :: Effect Unit
main = launchAff_ $ liftEffect do
  doc <- window >>= document
  elem_ <- toNonElementParentNode >>> getElementById "hide-elem" $ doc -- Maybe elem
  fn <- do -- Event -> Effect a
    eventListener $ \evt -> log "fn, Clicky"

  fn1 <- do -- Event -> Effect a
    eventListener $ \evt -> log "fn1, Clicky"

  -- If you add same EventListener to same element it will not be added 2 times. or called 2 times. Only onced.

  fromMaybe ignore $ addClickEvent fn <$> elem_

  case elem_ of
       Nothing -> ignore
       Just elem -> addClickEvent fn1 elem


  log "Main finished."



  -- Evt.addEventListener
  -- Evt.removeEventListener
  -- fadeToggle_ elem_
  -- toggleHidden_ elem_
  -- fadeIn_ elem_ -- Maybe Effect unit


  -- elem1 <- fadeIn <$> elem
  -- fromMaybe "ok" <$> elem1 >>> pure
  -- log


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
--        liftEffect $ logShow result


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
-- main = D.trace "hello" \_ -> log "there"
-- main = D.traceM $ "hello3"
-- main = D.spy "hello" $ log "there"


-- main :: Effect Unit
-- main = getById "Couldn't find anything" "hide-elem" >>= log
