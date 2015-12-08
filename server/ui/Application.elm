module Application (Model, Action, init, update, view) where

import Effects exposing (Never)
import Html exposing (div, text)

import Video
import VideoPlayer
import Util exposing (listFromMaybe)

type alias Model =
  { errorMessage: Maybe String
  , video: Maybe Video.Model
  , player: Maybe VideoPlayer.Model
  }

type Action
  = VideoLoaded (Maybe Video.Model)
  | VideoPlayerAction VideoPlayer.Action
  | VideoAction Video.Action

init : (Model, Effects.Effects Action)
init = (Model Nothing Nothing Nothing, Video.load VideoLoaded)

update : Action -> Model -> (Model, Effects.Effects Action)
update msg model =
  case msg of
    VideoLoaded Nothing ->
      let
        model' = { model | errorMessage = Just "Could not load video" }
      in (model', Effects.none)

    VideoLoaded (Just video) ->
      let
        model' = { model | video = Just video, player = Just player }
        (player, fx) = VideoPlayer.init video.url
      in (model', Effects.map VideoPlayerAction fx)

    VideoPlayerAction act ->
      case model.player of
        Just player ->
          let
            (player', fx) = VideoPlayer.update act player
            model' = { model | player = Just player' }
          in (model', Effects.map VideoPlayerAction fx)
        Nothing -> -- Wait what?
          (model, Effects.none)

    VideoAction act ->
      case model.video of
        Just video ->
          let
            video' = Video.update act video
            model' = { model | video = Just video' }
          in (model', Effects.none)
        Nothing ->
          (model, Effects.none)

view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    flash =
      Maybe.map (\err -> text err) model.errorMessage
    playerAddr = Signal.forwardTo address VideoPlayerAction
    player =
      Maybe.map (\player -> VideoPlayer.view playerAddr player) model.player
    currentTime = Maybe.withDefault 0 (Maybe.map .currentTime model.player)
    videoAddr = Signal.forwardTo address VideoAction
    subtitles =
      Maybe.map
        (\video -> Video.subtitlesView videoAddr currentTime video)
        model.video
    children =
      listFromMaybe flash ++ listFromMaybe player ++ listFromMaybe subtitles
  in div [] children
