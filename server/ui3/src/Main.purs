module Main where

import Prelude

import Data.Either
import Data.Lens
import qualified Data.Maybe.Unsafe as Unsafe
import Data.Nullable (Nullable(), toMaybe)
import Control.Monad.Eff

import qualified DOM as DOM
import qualified DOM.HTML as DOM
import qualified DOM.HTML.Types as DOM
import qualified DOM.HTML.Window as DOM
import qualified DOM.Node.ParentNode as DOM
import qualified React as R
import qualified React.DOM as R
import qualified React.DOM.Props as RP
import qualified Thermite as T

import Definitions
import VideoPlayer ()

-- Our application's state.
type State = { player :: VideoPlayer.State }

-- The actions we can perform on our application.
data Action = PlayerAction VideoPlayer.Action

_PlayerAction :: PrismP Action VideoPlayer.Action
_PlayerAction = prism PlayerAction \pa ->
  case pa of
    PlayerAction act -> Right act

initialState :: State
initialState = { player: VideoPlayer.initialState "vid" "/video.mp4" }

_player :: LensP State VideoPlayer.State
_player = lens _.player (_ { player = _ })

hello :: forall props. T.Spec AppEffects State props Action
hello
  = T.simpleSpec performAction render
  <> T.focus _player _PlayerAction VideoPlayer.videoPlayer
  <> T.focus _player _PlayerAction testControls

render :: forall props. T.Render State props Action
render dispatch _ state _ =
  [ R.p [ RP.key "hello" ] [ R.text "Hello, world!" ] ]

performAction :: forall props. T.PerformAction AppEffects State props Action
performAction _ _ state k = pure unit

unsafeFromNullable :: forall a. Nullable a -> a
unsafeFromNullable = Unsafe.fromJust <<< toMaybe

testControls :: forall props.
  T.Spec AppEffects VideoPlayer.State props VideoPlayer.Action
testControls = T.simpleSpec T.defaultPerformAction renderTestControls

renderTestControls :: forall props.
  T.Render VideoPlayer.State props VideoPlayer.Action
renderTestControls dispatch _ state _ =
  [ R.p' [ button "btn-play" VideoPlayer.Play "Play"
         , button "btn-pause" VideoPlayer.Pause "Pause"
         , button "btn-toggle" VideoPlayer.TogglePlay "Toggle"
         , R.text (show state.currentTime)
         ]
  ]
  where
    button key act label =
      R.button [ RP.key key, RP.onClick \_ -> dispatch act ] [ R.text label ]

main :: Eff (dom :: DOM.DOM) Unit
main = void do
  let component = T.createClass hello initialState
  document <- DOM.window >>= DOM.document
  let root = DOM.htmlDocumentToParentNode document
  body <- unsafeFromNullable <$> DOM.querySelector "#react" root
  R.render (R.createFactory component {}) body
