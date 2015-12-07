module VideoPlayer (Model, Action, init, update, view) where

import Effects exposing (Never)
import Html exposing (video, div, p, text)
import Html.Attributes exposing (src, controls)
import Html.Events exposing (on)
import Json.Decode as Json

-- Video player state.  Need to know (here or elsewhere) about load state,
-- playing/paused, current playback time.
type alias Model =
  { url: String
  , currentTime: Float
  }

type Action = Play | Pause | TimeUpdate Float

init : String -> (Model, Effects.Effects Action)
init url = (Model url 0, Effects.none)

update : Action -> Model -> (Model, Effects.Effects Action)
update msg model =
  case msg of
    TimeUpdate time ->
      ({ model | currentTime = time }, Effects.none)
    _ -> (model, Effects.none)

view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    ontimeupdate = onTimeUpdate (Signal.forwardTo address TimeUpdate)
  in video [ src model.url, controls True, ontimeupdate ] []

onTimeUpdate : Signal.Address Float -> Html.Attribute
onTimeUpdate address =
  on "timeupdate"
    (Json.at ["target", "currentTime"] Json.float)
    (\time -> Signal.message address time)
