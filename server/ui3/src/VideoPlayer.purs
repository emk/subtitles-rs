module VideoPlayer (
  State(), initialState,
  Action(Play, Pause, TogglePlay, PlayInterval, Seek, SeekRelative),
  videoPlayer
  ) where

import Prelude

import Control.Monad.Eff.Console
import Data.Maybe

import qualified React.DOM as R
import qualified React.DOM.Props as RP
import qualified Thermite as T

import Definitions

type State =
  { url :: String
  , size :: Size
  , playing :: Boolean
  , currentTime :: Time
  , stopAt :: Maybe Time
  }

initialState :: String -> State
initialState url =
  { url: url
  , size: Size 0 0
  , playing: false
  , currentTime: Time 0.0
  , stopAt: Nothing
  }

data Action
  = LoadedMetadata Size
  | TimeUpdate Time
  | PlayingUpdate Boolean
  | Play
  | Pause
  | TogglePlay
  | PlayInterval Interval
  | Seek Time
  | SeekRelative Time

videoPlayer :: forall props. T.Spec (AppEffects ()) State props Action
videoPlayer = T.simpleSpec performAction render

render :: forall props. T.Render State props Action
render dispatch _ state _ =
  [ R.video
    [ RP.key "vid"
    , RP.src state.url
    , RP.controls "controls"
    , onLoadedMetadata \e -> dispatch (LoadedMetadata (targetSize e))
    , onTimeUpdate \e -> dispatch (TimeUpdate (targetCurrentTime e))
    , onPlay \e -> dispatch (PlayingUpdate true)
    , onPause \e -> dispatch (PlayingUpdate false)
    ] [] ]

performAction :: forall props. T.PerformAction (AppEffects ()) State props Action

performAction (LoadedMetadata size) _ state k = do
  log $ "Setting size: " ++ show size
  k $ state { size = size }

performAction (TimeUpdate time) _ state k = do
  log $ "Setting time: " ++ show time
  k $ state { currentTime = time }

performAction (PlayingUpdate playing) _ state k = do
  log $ "Setting playing: " ++ show playing
  k $ state { playing = playing }

performAction _ _ state k = pure unit
