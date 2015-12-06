module Application (Model, Action, init, update, view) where

import Effects exposing (Never)
import Html exposing (div, text)
import Http
import Json.Decode as Json exposing ((:=))
import Task

import Video
import VideoPlayer

type alias Model =
  { errorMessage: Maybe String
  , video: Maybe Video.Model
  , player: Maybe VideoPlayer.Model
  }

type Action
  = VideoLoaded (Maybe Video.Model)
  | VideoPlayerAction VideoPlayer.Action

init : (Model, Effects.Effects Action)
init = (Model Nothing Nothing Nothing, loadVideo)

update : Action -> Model -> (Model, Effects.Effects Action)
update msg model =
  case msg of
    VideoLoaded Nothing ->
      let
        newModel = { model | errorMessage = Just "Could not load video" }
      in (newModel, Effects.none)

    VideoLoaded (Just video) ->
      let
        newModel = { model | video = Just video, player = Just player }
        (player, fx) = VideoPlayer.init video.url
      in (newModel, Effects.map VideoPlayerAction fx)

    VideoPlayerAction act ->
      case model.player of
        Just player ->
          let
            (newPlayer, fx) = VideoPlayer.update act player
            newModel = { model | player = Just newPlayer }
          in (newModel, Effects.map VideoPlayerAction fx)
        Nothing -> -- Wait what?
          (model, Effects.none)

view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    flash =
      case model.errorMessage of
        Just err -> [text err]
        Nothing -> []
    player =
      case model.player of
        Just player ->
          [VideoPlayer.view (Signal.forwardTo address VideoPlayerAction) player]
        Nothing -> []
  in div [] (flash ++ player)

decodeVideo : Json.Decoder Video.Model
decodeVideo =
  Json.object1 Video.Model
    ("url" := Json.string)

videoUrl : String
videoUrl = "/api/v1/video.json"

loadVideo : Effects.Effects Action
loadVideo =
  Http.get decodeVideo videoUrl
    |> Task.toMaybe
    |> Task.map VideoLoaded
    |> Effects.task


