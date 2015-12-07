module Video (Model, init, subtitleView, decode, load) where

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

subtitleView : Float -> Model -> Html.Html
subtitleView currentTime model =
  let
    idx = Subtitle.timeToIndex currentTime model.subtitles
    prev = Array.get (idx - 1) model.subtitles
    curr = Array.get idx model.subtitles
    next = Array.get (idx + 1) model.subtitles
    subs = listFromMaybe prev ++ listFromMaybe curr ++ listFromMaybe next
  in div [class "subtitles"] (List.map Subtitle.view subs)

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
