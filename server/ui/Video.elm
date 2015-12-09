module Video
  (Model, init, Action, keyPress, update, playerView, subtitlesView, inputs,
   decode, load
  ) where

import Debug exposing (log)
import Effects
import Html exposing (div, text, button)
import Html.Attributes exposing (class, style, classList)
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
  , playerHeight: Int
  , mode: DisplayMode
  , subtitles: Subtitle.Array.Model
  }

init : String -> Subtitle.Array.Model -> (Model, Effects.Effects Action)
init url subtitles =
  let (player, fx) = VideoPlayer.init url
  in (Model url player 0 Current subtitles, Effects.map Player fx)

type Action
  = Player VideoPlayer.Action
  | Subtitles Subtitle.Array.Action
  | SetMode DisplayMode
  | KeyPress Int
  | Arrows { x: Int, y: Int }
  | PlayerHeight Int

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
    PlayerHeight height ->
      ({ model | playerHeight = log "Video.playerHeight" height }, Effects.none)

playerView : Signal.Address Action -> Model -> Html.Html
playerView address model =
  VideoPlayer.view (Signal.forwardTo address Player) model.player

subtitlesView : Signal.Address Action -> Model -> Html.Html
subtitlesView address model =
  let
    currentTime = model.player.currentTime
    current = Subtitle.Array.maybeIndexFromTime currentTime model.subtitles
    indicies = subtitlesToShow model
    playerAddr = (Signal.forwardTo address Player)
    addr = Signal.forwardTo address Subtitles
    subtitles =
      div [class "subtitles-list"]
        (Subtitle.Array.viewsAt
          indicies current playerAddr addr model.subtitles)
    styles = style [("top", toString model.playerHeight ++ "px")]
  in
    div [class "subtitles-view", styles]
      ([buttonBar address model] ++ [div [class "scroll"] [subtitles]])

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

buttonBar : Signal.Address Action -> Model -> Html.Html
buttonBar address model =
  let
    onclick mode = onClick address (SetMode mode)
    btnClasses mode =
      classList
        [ ("btn", True)
        , ("btn-default", True)
        , ("active", mode == model.mode)
        ]
    currentButton =
      button [onclick Current, btnClasses Current] [text "Current"]
    selectedButton =
      button [onclick Selected, btnClasses Selected] [text "Selected"]
    classes = class "button-bar btn-group btn-group-sm"
  in div [classes] [currentButton, selectedButton]

inputs : List (Signal.Signal Action)
inputs =
  [ Signal.map KeyPress Keyboard.presses
  , Signal.map Arrows Keyboard.arrows
  , Signal.map PlayerHeight VideoPlayer.height
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
