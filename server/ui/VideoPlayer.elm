module VideoPlayer
  (Model, DomAction, Action, play, pause, togglePlay, seek, seekRelative,
  init, update, view, playerMailbox, height
  ) where

import Basics exposing (min)
import Effects exposing (Never)
import Html exposing (video, div, p, text)
import Html.Attributes exposing (src, controls)
import Html.Events exposing (on)
import Json.Decode
import Json.Encode
import Signal
import Task
import Window

import Util exposing (Size, targetSize)

-- Video player state.  Need to know (here or elsewhere) about load state,
-- playing/paused, current playback time.
type alias Model =
  { url: String
  , size: Size
  , currentTime: Float
  , playing: Bool
  }

type alias DomAction =
  { command: String
  , value:  Json.Encode.Value
  }

type Action
  = LoadedMetadata Size
  | TimeUpdate Float
  | PlayingUpdate Bool
  | Dom DomAction
  | TogglePlay
  | SeekRelative Float
  | TaskDone

play : Action
play = Json.Encode.null |> DomAction "play" |> Dom

pause : Action
pause = Json.Encode.null |> DomAction "pause" |> Dom

togglePlay : Action
togglePlay = TogglePlay

seek : Float -> Action
seek time = Json.Encode.float time |> DomAction "seek" |> Dom

seekRelative : Float -> Action
seekRelative = SeekRelative

init : String -> (Model, Effects.Effects Action)
init url = (Model url (Size 0 0) 0 False, Effects.none)

update : Action -> Model -> (Model, Effects.Effects Action)
update msg model =
  case msg of
    LoadedMetadata size ->
      ({ model | size = size },
       Signal.send sizeMailbox.address size
         |> Task.map (\_ -> TaskDone)
         |> Effects.task)
    TimeUpdate time ->
      ({ model | currentTime = time }, Effects.none)
    PlayingUpdate playing ->
      ({ model | playing = playing}, Effects.none)
    Dom domAction ->
      let
        fx =
          Signal.send playerMailbox.address domAction
            |> Task.map (\_ -> TaskDone)
            |> Effects.task
      in (model, fx)
    TogglePlay ->
      if model.playing then
        update pause model
      else
        update play model
    SeekRelative offset ->
      update (seek (model.currentTime + offset)) model
    TaskDone -> (model, Effects.none)

view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    onloaded =
      onLoadedMetadata <| \size -> Signal.message address (LoadedMetadata size)
    ontimeupdate = onTimeUpdate (Signal.forwardTo address TimeUpdate)
    onplay = onPlay (Signal.message address (PlayingUpdate True))
    onpause = onPause (Signal.message address (PlayingUpdate False))
  in
    video
      [ src model.url, controls True
      , onloaded, ontimeupdate, onplay, onpause
      ]
      []

onLoadedMetadata : (Size -> Signal.Message) -> Html.Attribute
onLoadedMetadata toMessage =
  on "loadedmetadata" targetSize toMessage

onTimeUpdate : Signal.Address Float -> Html.Attribute
onTimeUpdate address =
  on "timeupdate"
    (Json.Decode.at ["target", "currentTime"] Json.Decode.float)
    (\time -> Signal.message address time)

onPlay : Signal.Message -> Html.Attribute
onPlay message =
  on "play" Json.Decode.value (\_ -> message)

onPause : Signal.Message -> Html.Attribute
onPause message =
  on "pause" Json.Decode.value (\_ -> message)

playerMailbox : Signal.Mailbox DomAction
playerMailbox = Signal.mailbox (DomAction "none" Json.Encode.null)

-- Hack. See below.
sizeMailbox : Signal.Mailbox Size
sizeMailbox = Signal.mailbox (Size 0 0)

-- The height of our video player.  This is special-cased because
-- `Window.width` doesn't work correctly with the Elm Architecture, as
-- discussed here:
--
-- https://groups.google.com/forum/#!topic/elm-discuss/4uQu6BgyLIE
--
-- Basically, we'll never receive an initial value for `Window.width`.  But
-- if we combine it with another signal, we'll at least receive an update
-- whenever that signal changes.  This is a major design flaw in
-- `startApp`, and possibly in Elm itself, so hackery is OK.
height : Signal.Signal Int
height = Signal.map2 calculateHeight Window.width sizeMailbox.signal

calculateHeight : Int -> Size -> Int
calculateHeight width playerSize =
  let
    scale = (toFloat width - 30) / toFloat playerSize.width
    height = ceiling (min scale 1.0 * toFloat playerSize.height)
  in height
