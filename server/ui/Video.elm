module Video (Model, init, Action, update, subtitlesView, decode, load) where

import Array
import Effects
import Html exposing (div)
import Html.Attributes exposing (class)
import Http
import Json.Decode as Json exposing ((:=))
import Task

import Subtitle
import SubtitleArray
import Util exposing (listFromMaybe)

type alias Model = { url: String, subtitles: Array.Array Subtitle.Model }

init : String -> Array.Array Subtitle.Model -> Model
init url subtitles = Model url subtitles

type Action = Subtitles SubtitleArray.Action

update : Action -> Model -> Model
update action model =
  case action of
    Subtitles act ->
      { model | subtitles = SubtitleArray.update act model.subtitles }

subtitlesView : Signal.Address Action -> Float -> Model -> Html.Html
subtitlesView address currentTime model =
  let
    idx = SubtitleArray.timeToIndex currentTime model.subtitles
    addr = Signal.forwardTo address Subtitles
    prev = SubtitleArray.viewAt (idx - 1) addr model.subtitles
    curr = SubtitleArray.viewAt idx addr model.subtitles
    next = SubtitleArray.viewAt (idx + 1) addr model.subtitles
    subs = listFromMaybe prev ++ listFromMaybe curr ++ listFromMaybe next
  in div [class "subtitles"] subs

decode : Json.Decoder Model
decode =
  Json.object2 Model
    ("url" := Json.string)
    ("subtitles" := Json.array Subtitle.decode)

videoUrl : String
videoUrl = "/api/v1/video.json"

load : (Maybe Model -> a) -> Effects.Effects a
load toAction =
  Http.get decode videoUrl
    |> Task.toMaybe
    |> Task.map toAction
    |> Effects.task
