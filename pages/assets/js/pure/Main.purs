module Main where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (fst, snd)
import Data.Unit (unit)
import Debug.Trace (spy, trace, traceM)
import Effect (Effect)
import Effect.Aff (Aff, delay, forkAff, joinFiber, launchAff, launchAff_)
import Effect.Aff.Bus (make, read, split, write)
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
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core as J
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))


-- Note: if you want default value in function, use record {}


-- Note: Use AX.request for using headers, or more tailored request.
-- If you don't need to use headers, or you want to put them to uri, use GET POST etc.
--
-- AX.GE POST etc. have variantion for ignoring response.


main :: Effect Unit
main = launchAff_ do
  result <- AX.request (AX.defaultRequest { url = "https://dog.ceo/api/breeds/image/random", method = Left GET, responseFormat = ResponseFormat.json })
  case result of
    Left err -> liftEffect $ log $ "GET /api response failed to decode: " <> AX.printError err
    Right response -> liftEffect $ log $ "GET /api response: " <> J.stringify response.body





-- There is import Web.DOM.MutationObserver have a look how to implement it. It might be exactly what I need.
-- Have a look on how to implement simple ( throttle. )
-- Have a look on how to implement simple delay.
-- Have a look on how to implement simple debounce.

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
  case htmlele_ of
    (Nothing) -> log "Didn't find element"
    (Just htmlele) -> do
                      b <- hidden htmlele
                      a <- case b of
                        true -> setHidden false htmlele
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


-- main :: Effect Unit
-- main = launchAff_ $ liftEffect do
--   log "works"
--
--

getElem :: Effect (Maybe Element)
getElem = do
  window >>= document >>= toNonElementParentNode >>> (getElementById "hide-elem")


hook bus = do
  promise <- forkAff $ do
    text <- read bus
    liftEffect $ traceM $ text <> "from loop"
    hook bus
  joinFiber promise


hook2 bus = do
  promise <- forkAff $ do
    text <- read bus
    liftEffect $ traceM $ text <> "from loop2"
    hook2 bus
  joinFiber promise

-- main :: Effect Unit
-- main = launchAff_ do
--   bus <- make -- Initalize buss. -> BusRW a.
--   elem_ <- liftEffect getElem
--   fn <- do -- Event -> Effect a
--     liftEffect $ eventListener $ \evt ->
--       launchAff_ do
--         promise2 <- forkAff $ write "Somthing from BUS" bus
--         res2 <- joinFiber promise2
--         liftEffect $ ignore

--   liftEffect $ fromMaybe ignore $ addClickEvent fn <$> elem_
--   res5p <- forkAff $ hook bus -- this will start blocking.
--   res6p <- forkAff $ hook2 bus
--   -- promise4 <- forkAff $ do
--   --   text <- read bus
--   --   liftEffect $ traceM $ text <> " so it is not same"


--   -- promise3 <- forkAff $ write "new value from the bus" bus


--   -- res1 <- joinFiber promise1
--   -- res4 <- joinFiber promise4

--   -- res2 <- joinFiber promise2
--   -- res3 <- joinFiber promise3
--   -- liftEffect $ traceM res1
--   -- liftEffect $ traceM res2
--   -- liftEffect $ traceM res3
--   -- liftEffect $ traceM res4
--   -- liftEffect $ traceM res5
--   liftEffect $ log "Done"
--   res5 <- joinFiber res5p
--   res6 <- joinFiber res6p
--   ignore




  
-- joinFiber


  -- write "something new" bus
  -- text <- read bus

  -- liftEffect $ traceM $ "Welcome"
  -- bus <- make -- Initalize buss. -> BusRW a.
  -- liftEffect $ traceM $ bus
  -- liftEffect $ traceM $ "Read bus"
  -- liftEffect $ traceM $ read bus
  -- liftEffect $ traceM $ "Bus 1"
  -- liftEffect $ traceM $ bus
  -- liftEffect $ traceM $ "Write bus"
  -- liftEffect $ traceM $ write "Hello from the bus" bus
  -- liftEffect $ traceM $ "Bus 2"
  -- liftEffect $ traceM $ bus
  -- liftEffect $ traceM $ "Read bus 3"
  -- liftEffect $ traceM $ read bus
  -- liftEffect $ traceM $ "Bus  4"
  -- liftEffect $ traceM $ bus




  -- bus <- make -- Initalize buss. -> BusRW a.
  -- write "something new" bus
  -- text <- read bus
  -- liftEffect $ log text
  -- liftEffect $ spy "show bus" $ traceM bus
  -- liftEffect $ spy "read bus" $ traceM $ read bus
  -- liftEffect $ spy "Bus 1" $ traceM $ bus
  -- liftEffect $ spy "Write bus" $ traceM $ write "Hello from the bus" bus
  -- liftEffect $ spy "Bus 2" $ traceM $ bus
  -- liftEffect $ spy "read bus 2" $ traceM $ read bus
  -- liftEffect $ spy "Bus 3" $ traceM $ bus

  -- bus
  -- read bus
  -- pure bus
  -- write "Hello from the bus" bus
  -- pure bus
  -- read bus
  -- pure bus


  -- liftEffect $ spy "show bus" $ traceM bus
  -- liftEffect $ spy "read bus" $ traceM $ read bus
  -- liftEffect $ spy "Bus 1" $ traceM $ bus
  -- liftEffect $ spy "Write bus" $ traceM $ write "Hello from the bus" bus
  -- liftEffect $ spy "Bus 2" $ traceM $ bus
  -- liftEffect $ spy "read bus 2" $ traceM $ read bus
  -- liftEffect $ spy "Bus 3" $ traceM $ bus
  -- traceM $ read bus
  -- trace "write bus"
  -- write "Hello from the bus" bus
  -- that <- read bus
  -- traceM bus
  -- traceM bus
  -- write "Hello from the bus2" bus
  -- liftEffect $ traceM that
  -- liftEffect $ traceM that
  -- liftEffect $ traceM bus




  -- fn <- do -- Event -> Effect a
  --   eventListener $ \evt -> log "fn, Clicky"
  -- fn1 <- do -- Event -> Effect a
  --   eventListener $ \evt -> log "fn1, Clicky"
  -- fromMaybe ignore $ addClickEvent fn <$> elem_
  -- fromMaybe ignore $ addClickEvent fn <$> elem2_
  -- case elem_ of
  --      Nothing -> ignore
  --      Just elem -> addClickEvent fn1 elem
  -- log "Main finished."



-- main :: Effect Unit
-- main = launchAff_ $ liftEffect do
--   doc <- window >>= document
--   elem_ <- toNonElementParentNode >>> getElementById "hide-elem" $ doc -- Maybe elem
--   elem2_ <- toNonElementParentNode >>> getElementById "hide-elem2" $ doc -- Maybe elem
--   fn <- do -- Event -> Effect a
--     eventListener $ \evt -> log "fn, Clicky"

--   fn1 <- do -- Event -> Effect a
--     eventListener $ \evt -> log "fn1, Clicky"

--   -- If you add same EventListener to same element it will not be added 2 times. or called 2 times. Only onced.
--   -- But if you add same EventListener into 2 different elements, then it would be called correctly 2 times.

--   fromMaybe ignore $ addClickEvent fn <$> elem_

--   fromMaybe ignore $ addClickEvent fn <$> elem2_

--   case elem_ of
--        Nothing -> ignore
--        Just elem -> addClickEvent fn1 elem


--   log "Main finished."



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
