module Test.Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "You should add some tests."
