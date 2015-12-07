module Video (Model, init, decode, load) where

import Effects
import Http
import Json.Decode as Json exposing ((:=))
import Task

import Subtitle

type alias Model = { url: String, subtitles: List Subtitle.Model }

init : String -> List Subtitle.Model -> Model
init url subtitles = Model url subtitles

decode : Json.Decoder Model
decode =
  Json.object2 Model
    ("url" := Json.string)
    ("subtitles" := Json.list Subtitle.decode)

videoUrl : String
videoUrl = "/api/v1/video.json"

load : (Maybe Model -> a) -> Effects.Effects a
load toAction =
  Http.get decode videoUrl
    |> Task.toMaybe
    |> Task.map toAction
    |> Effects.task
