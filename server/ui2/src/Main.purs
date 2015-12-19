module Main where

import Prelude

import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.Eff.Exception (throwException)

import Data.Functor.Coproduct (Coproduct())
import Data.Generic (Generic, gEq, gCompare)

import Halogen
import Halogen.Util (appendToBody)
import qualified Halogen.HTML.Indexed as H
--import qualified Halogen.HTML.Properties.Indexed as P

import Definitions
import VideoPlayer (videoPlayer)

-- Our application's state.
data State = State

-- Ugh I can't believe this type machinery.
type ChildState = VideoPlayer.State
type ChildQuery = VideoPlayer.Query
data ChildSlot = VideoPlayerSlot
type StateP g = InstalledState State ChildState Query ChildQuery g ChildSlot
type QueryP = Coproduct Query (ChildF ChildSlot ChildQuery)
derive instance genericChildSlot :: Generic ChildSlot
instance eqChildSlot :: Eq ChildSlot where eq = gEq
instance ordChildSlot :: Ord ChildSlot where compare = gCompare

-- Useless command for now.
data Query a = Hello a

-- Define our main UI component.
ui :: Component (StateP AppAff) QueryP AppAff
ui = parentComponent render eval
  where

    -- Render our UI component.
    render :: State -> ParentHTML ChildState Query ChildQuery AppAff ChildSlot
    render state = H.div_
      [ H.slot VideoPlayerSlot $ \_ ->
         { component: videoPlayer
         , initialState: VideoPlayer.initialState "/video.mp4"
         }
      ]

    -- Process our supported queries.
    eval :: Natural Query (ParentDSL State ChildState Query ChildQuery AppAff ChildSlot)
    eval (Hello next) = do
      pure next

main :: Eff (AppEffects ()) Unit
main = runAff throwException (const (pure unit)) do
  liftEff $ log "Starting app"
  app <- runUI ui (installedState State)
  appendToBody app.node
