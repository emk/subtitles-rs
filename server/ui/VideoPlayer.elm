module VideoPlayer
  (Model, DomAction, Action, play, pause, togglePlay, seek, seekRelative,
  init, update, view, playerMailbox
  ) where

import Effects exposing (Never)
import Html exposing (video, div, p, text)
import Html.Attributes exposing (src, controls)
import Html.Events exposing (on)
import Json.Decode
import Json.Encode
import Signal
import Task

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
      ({ model | size = size }, Effects.none)
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
