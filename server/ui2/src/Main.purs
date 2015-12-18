module Main where

import Prelude

import Control.Monad.Aff (Aff(), runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.Eff.Exception (throwException)

import Halogen
import Halogen.Util (appendToBody)
import qualified Halogen.HTML.Indexed as H
--import qualified Halogen.HTML.Properties.Indexed as P

-- The effects used by our application.
type AppEffects eff = HalogenEffects (console :: CONSOLE | eff)

-- The monad in which we'll run most of our computations.
type AppAff = Aff (AppEffects ())

-- Empty state for now.
data State = State

-- Useless commands.
data Query a = Hello a

-- Define our main UI component.
ui :: Component State Query AppAff
ui = component render eval
  where

    -- Render our UI component.
    render :: State -> ComponentHTML Query
    render state = H.p_ [ H.text "Hello!" ]

    -- Process our supported queries.
    eval :: Natural Query (ComponentDSL State Query AppAff)
    eval (Hello next) = do
      liftEff' $ log "Hello to you to!"
      pure next

main :: Eff (AppEffects ()) Unit
main = runAff throwException (const (pure unit)) do
  liftEff $ log "Starting app"
  app <- runUI ui State
  appendToBody app.node
