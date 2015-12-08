module Video
  (Model, init, Action, update, playerView, subtitlesView, decode, load) where

import Effects
import Html exposing (div)
import Html.Attributes exposing (class)
import Http
import Json.Decode as Json exposing ((:=))
import Task

import Subtitle
import Subtitle.Array
import Util exposing (listFromMaybe, updateChild)
import VideoPlayer

type alias Model =
  { url: String
  , player: VideoPlayer.Model
  , subtitles: Subtitle.Array.Model
  }

init : String -> Subtitle.Array.Model -> (Model, Effects.Effects Action)
init url subtitles =
  let
    (player, fx) = VideoPlayer.init url
  in (Model url player subtitles, Effects.map Player fx)

type Action
  = Player VideoPlayer.Action
  | Subtitles Subtitle.Array.Action

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    Player act ->
      updateChild act model.player VideoPlayer.update Player
        (\p -> { model | player = p })
    Subtitles act ->
      updateChild act model.subtitles Subtitle.Array.update Subtitles
        (\subs -> { model | subtitles = subs })

playerView : Signal.Address Action -> Model -> Html.Html
playerView address model =
  VideoPlayer.view (Signal.forwardTo address Player) model.player

subtitlesView : Signal.Address Action -> Model -> Html.Html
subtitlesView address model =
  let
    currentTime = model.player.currentTime
    idx = Subtitle.Array.timeToIndex currentTime model.subtitles
    addr = Signal.forwardTo address Subtitles
    prev = Subtitle.Array.viewAt (idx - 1) addr model.subtitles
    curr = Subtitle.Array.viewAt idx addr model.subtitles
    next = Subtitle.Array.viewAt (idx + 1) addr model.subtitles
    subs = listFromMaybe prev ++ listFromMaybe curr ++ listFromMaybe next
  in div [class "subtitles"] subs

decode : Json.Decoder (Model, Effects.Effects Action)
decode =
  Json.object2 init
    ("url" := Json.string)
    ("subtitles" := Json.array Subtitle.decode)

videoUrl : String
videoUrl = "/api/v1/video.json"

load : (Maybe (Model, Effects.Effects Action) -> a) -> Effects.Effects a
load toAction =
  Http.get decode videoUrl
    |> Task.toMaybe
    |> Task.map toAction
    |> Effects.task
