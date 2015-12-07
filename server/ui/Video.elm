module Video (Model, init, Action, update, subtitlesView, decode, load) where

import Array
import Effects
import Html exposing (div)
import Html.Attributes exposing (class)
import Http
import Json.Decode as Json exposing ((:=))
import Task

import Subtitle
import Util exposing (listFromMaybe)

type alias Model = { url: String, subtitles: Array.Array Subtitle.Model }

init : String -> Array.Array Subtitle.Model -> Model
init url subtitles = Model url subtitles

type Action = SubtitleN Int Subtitle.Action

update : Action -> Model -> Model
update action model =
  case action of
    SubtitleN idx act ->
      case Array.get idx model.subtitles of
        Just sub ->
          let
            newSub = Subtitle.update act sub
          in { model | subtitles = Array.set idx newSub model.subtitles }
        Nothing -> model -- Huh? Shouldn't happen.

subtitlesView : Signal.Address Action -> Float -> Model -> Html.Html
subtitlesView address currentTime model =
  let
    view i ms =
      Maybe.map
        (\s -> Subtitle.view (Signal.forwardTo address (SubtitleN i)) s)
        ms
    idx = Subtitle.timeToIndex currentTime model.subtitles
    prev = view (idx - 1) (Array.get (idx - 1) model.subtitles)
    curr = view idx (Array.get idx model.subtitles)
    next = view (idx + 1) (Array.get (idx + 1) model.subtitles)
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
