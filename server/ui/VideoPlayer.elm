module VideoPlayer (Model, Action, init, update, view) where

import Effects exposing (Never)
import Html exposing (video)
import Html.Attributes exposing (src, controls)

-- Video player state.  Need to know (here or elsewhere) about load state,
-- playing/paused, current playback time.
type alias Model = { url: String }

type Action = Play | Pause

init : String -> (Model, Effects.Effects Action)
init url = (Model url, Effects.none)

update : Action -> Model -> (Model, Effects.Effects Action)
update msg model = (model, Effects.none)

view : Signal.Address Action -> Model -> Html.Html
view address model = video [ src model.url, controls True ] []
