module Video
  (Model, init, Action, keyPress, update, playerView, subtitlesView, inputs,
   decode, load
  ) where

import Effects
import Html exposing (div)
import Html.Attributes exposing (class)
import Http
import Json.Decode as Json exposing ((:=))
import Keyboard
import Task

import Subtitle
import Subtitle.Array
import Util exposing (listFromMaybes, updateChild)
import VideoPlayer

type alias Model =
  { url: String
  , player: VideoPlayer.Model
  , subtitles: Subtitle.Array.Model
  , currentSubtitle: Maybe Int
  }

init : String -> Subtitle.Array.Model -> (Model, Effects.Effects Action)
init url subtitles =
  let (player, fx) = VideoPlayer.init url
  in (Model url player subtitles Nothing, Effects.map Player fx)

type Action
  = Player VideoPlayer.Action
  | Subtitles Subtitle.Array.Action
  | KeyPress Int
  | Arrows { x: Int, y: Int }

keyPress : Int -> Action
keyPress = KeyPress

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    Player act ->
      let
        (model', fx) =
          updateChild act model.player VideoPlayer.update Player
            (\p -> { model | player = p })
        currentTime = model'.player.currentTime
        current = Subtitle.Array.maybeIndexFromTime currentTime model.subtitles
        model'' = { model' | currentSubtitle = current }
      in (model'', fx)
    Subtitles act ->
      updateChild act model.subtitles Subtitle.Array.update Subtitles
        (\subs -> { model | subtitles = subs })
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
    current = model.currentSubtitle
    currentTime = model.player.currentTime
    idx = Subtitle.Array.indexFromTime currentTime model.subtitles
    indicies = [(idx - 1), idx, (idx + 1)]
    playerAddr = (Signal.forwardTo address Player)
    addr = Signal.forwardTo address Subtitles
    children =
      Subtitle.Array.viewsAt
        indicies current playerAddr addr model.subtitles
  in div [class "subtitles"] children

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
