module Video (Model, init) where

import Subtitle

type alias Model = { url: String, subtitles: List Subtitle.Model }

init : String -> List Subtitle.Model -> Model
init url subtitles = Model url subtitles
