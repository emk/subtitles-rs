module Application (Model, Action, init, update, view, inputs) where

import Effects exposing (Never)
import Html exposing (div, text)
import Html.Attributes exposing (id, class)

import Video
import Util exposing (listFromMaybes, maybeUpdateChild)

type alias Model =
  { errorMessage: Maybe String
  , video: Maybe Video.Model
  }

type Action
  = VideoLoaded (Maybe (Video.Model, Effects.Effects Video.Action))
  | VideoAction Video.Action

init : (Model, Effects.Effects Action)
init = (Model Nothing Nothing, Video.load VideoLoaded)

update : Action -> Model -> (Model, Effects.Effects Action)
update msg model =
  case msg of
    VideoLoaded Nothing ->
      let
        model' = { model | errorMessage = Just "Could not load video" }
      in (model', Effects.none)

    VideoLoaded (Just (video, fx)) ->
      ({ model | video = Just video }, Effects.map VideoAction fx)

    VideoAction act ->
      maybeUpdateChild act model.video Video.update VideoAction model
        (\v -> { model | video = Just v })

view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    -- TODO: The layout of the flash is broken, because we don't
    -- reserve space for it.  We can probably fix this in CSS.  But it's
    -- not a priority because we currently can't have both a flash and
    -- a video.
    flash =
      Maybe.map (\err -> text err) model.errorMessage
    videoAddr = Signal.forwardTo address VideoAction
    player =
      Maybe.map (\video -> Video.playerView videoAddr video) model.video
    subtitles =
      Maybe.map (\video -> Video.subtitlesView videoAddr video) model.video
  in
    div [id "app"] (listFromMaybes [flash, player, subtitles])

inputs : List (Signal.Signal Action)
inputs = List.map (Signal.map VideoAction) Video.inputs
