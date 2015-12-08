module Video
  (Model, init, Action, keyPress, update, playerView, subtitlesView, inputs,
   decode, load
  ) where

import Effects
import Html exposing (div, text, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json exposing ((:=))
import Keyboard
import Task

import Subtitle
import Subtitle.Array
import Util exposing (listFromMaybes, updateChild)
import VideoPlayer

type DisplayMode = Current | Selected

type alias Model =
  { url: String
  , player: VideoPlayer.Model
  , mode: DisplayMode
  , subtitles: Subtitle.Array.Model
  }

init : String -> Subtitle.Array.Model -> (Model, Effects.Effects Action)
init url subtitles =
  let (player, fx) = VideoPlayer.init url
  in (Model url player Current subtitles, Effects.map Player fx)

type Action
  = Player VideoPlayer.Action
  | Subtitles Subtitle.Array.Action
  | SetMode DisplayMode
  | KeyPress Int
  | Arrows { x: Int, y: Int }

keyPress : Int -> Action
keyPress = KeyPress

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    Player act ->
      updateChild act model.player VideoPlayer.update Player
        (\p -> { model | player = p })
    Subtitles act ->
      updateChild act model.subtitles Subtitle.Array.update Subtitles
        (\subs -> { model | subtitles = subs })
    SetMode mode ->
      ({ model | mode = mode }, Effects.none)
    KeyPress keyCode ->
      case keyCode of
        32 -> update (VideoPlayer.togglePlay |> Player) model
        _ -> (model, Effects.none)
    Arrows arrows ->
      case (arrows.x, arrows.y) of
        (-1, 0) -> update (VideoPlayer.seekRelative -5 |> Player) model
        ( 1, 0) -> update (VideoPlayer.seekRelative  5 |> Player) model
        _ -> (model, Effects.none)

playerView : Signal.Address Action -> Model -> Html.Html
playerView address model =
  VideoPlayer.view (Signal.forwardTo address Player) model.player

subtitlesView : Signal.Address Action -> Model -> Html.Html
subtitlesView address model =
  let
    currentButton =
      button [onClick address (SetMode Current)] [text "Current"]
    selectedButton =
      button [onClick address (SetMode Selected)] [text "Selected"]
    currentTime = model.player.currentTime
    current = Subtitle.Array.maybeIndexFromTime currentTime model.subtitles
    indicies = subtitlesToShow model
    playerAddr = (Signal.forwardTo address Player)
    addr = Signal.forwardTo address Subtitles
    subtitles =
      Subtitle.Array.viewsAt
        indicies current playerAddr addr model.subtitles
  in div [class "subtitles"] ([currentButton, selectedButton] ++ subtitles)

subtitlesToShow : Model -> List Int
subtitlesToShow model =
  case model.mode of
    Current ->
      let
        currentTime = model.player.currentTime
        idx = Subtitle.Array.indexFromTime currentTime model.subtitles
      in [(idx - 1), idx, (idx + 1)]
    Selected ->
      Subtitle.Array.selectedIndices model.subtitles

inputs : List (Signal.Signal Action)
inputs =
  [ Signal.map KeyPress Keyboard.presses
  , Signal.map Arrows Keyboard.arrows
  ]

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
