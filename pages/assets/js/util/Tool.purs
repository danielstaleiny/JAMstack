module Tool where

import Prelude
import Effect (Effect)
import Effect.Console (log)

run :: Effect Unit
run = do
  log "Hey TOOL PURESCRIPT!"
