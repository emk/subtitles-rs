module VideoPlayer (
  State(), initialState,
  Action(Play, Pause, TogglePlay, PlayInterval, Seek, SeekRelative),
  videoPlayer
  ) where

import Prelude

import Control.Monad.Eff.Console
import Data.Maybe
import Data.Tuple
import Math (max)

import qualified React.DOM as R
import qualified React.DOM.Props as RP
import qualified Thermite as T

import Definitions
import VideoDOM

type State =
  { id :: String
  , url :: String
  , size :: Size
  , playing :: Boolean
  , currentTime :: Time
  , stopAt :: Maybe Time
  }

initialState :: String -> String -> State
initialState id url =
  { id: id
  , url: url
  , size: { width: 0, height: 0 }
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

videoPlayer :: forall props. T.Spec AppEffects State props Action
videoPlayer = T.simpleSpec performAction render

render :: forall props. T.Render State props Action
render dispatch _ state _ =
  [ R.video
    [ RP._id state.id
    , RP.key state.id
    , RP.src state.url
    , RP.controls "controls"
    , onLoadedMetadata \e -> dispatch (LoadedMetadata (targetSize e))
    , onTimeUpdate \e -> dispatch (TimeUpdate (targetCurrentTime e))
    , onPlay \e -> dispatch (PlayingUpdate true)
    , onPause \e -> dispatch (PlayingUpdate false)
    ] [] ]

performAction :: forall props. T.PerformAction AppEffects State props Action

performAction (LoadedMetadata size) _ state k = do
  log $ "Setting size: " ++ show size.width ++ "x" ++ show size.height
  k $ state { size = size }

performAction (TimeUpdate time) _ state k = do
  log $ "Setting time: " ++ show time
  case (Tuple state.stopAt state.currentTime) of
    Tuple (Just (Time stop)) (Time current) | current >= stop -> do
      pause state.id
      k $ state { currentTime = time, stopAt = Nothing }
    _ ->
      k $ state { currentTime = time }

performAction (PlayingUpdate playing) _ state k = do
  log $ "Setting playing: " ++ show playing
  k $ state { playing = playing }

performAction Play _ state k = do
  play state.id
  k state

performAction Pause _ state k = do
  pause state.id
  k state

performAction TogglePlay _ state k = do
  if state.playing
    then pause state.id
    else play state.id
  k state

performAction (PlayInterval interval) _ state k = do
  setCurrentTime state.id interval.begin
  if state.playing
    -- If we're already playing, don't bother with anything else.
    then k state
    -- Otherwise, also start playback and set end time.
    else do
      play state.id
      k $ state { stopAt = Just interval.end }

performAction (Seek time) _ state k = do
  setCurrentTime state.id time
  k state

performAction (SeekRelative (Time offset)) _ state k = do
  case state.currentTime of
    Time current ->
      setCurrentTime state.id (Time (max 0.0 (current + offset)))
