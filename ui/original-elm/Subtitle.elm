module Subtitle
  (Model, startTime, endTime, init, Action, update, view, decode) where

import Effects
import Html exposing (div, text, p, input)
import Html.Attributes exposing (class, classList, type', checked, property)
import Html.Events exposing (onClick)
import Json.Decode as Json exposing ((:=))
import Json.Encode
import Signal exposing (Address)
import Svg exposing (svg, use)
import Svg.Attributes exposing (xlinkHref, viewBox)

import Util exposing (listFromMaybes, checkbox)
import VideoPlayer

type alias Model =
  { period: (Float, Float)
  , foreignText: Maybe String
  , nativeText: Maybe String
  , selected: Bool
  }

startTime : Model -> Float
startTime model = fst model.period

endTime : Model -> Float
endTime model = snd model.period

init : (Float, Float) -> Maybe String -> Maybe String -> Model
init period foreignText nativeText =
  Model period foreignText nativeText False

type Action = Selected Bool

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    Selected val -> ({ model | selected = val }, Effects.none)

view
  : Address VideoPlayer.Action -> Address Action -> Bool -> Model
  -> Html.Html
view playerAddress address current model =
  let
    onclick = onClick playerAddress (VideoPlayer.playRange model.period)
    check = checkbox address model.selected Selected
    play =
      svg [viewBox "0 0 32 32", onclick] [use [xlinkHref "play.svg#play"] []]
    foreignHtml =
      model.foreignText |> Maybe.map (\t -> p [class "foreign"] [text t])
    nativeHtml =
      model.nativeText |> Maybe.map (\t -> p [class "native"] [text t])
    children = 
      [check, play] ++ listFromMaybes [foreignHtml, nativeHtml]
    -- We need this so the virtual-dom diffing code can tell subtitles
    -- apart.  Without it, it will mix up checkboxes in certain views,
    -- because their values don't show up in the DOM.
    key = property "key" (Json.Encode.string (toString model.period))
    classes = classList [("subtitle", True), ("current", current)]
  in div [key, classes] children

decode : Json.Decoder Model
decode =
  Json.object3 init
    ("period" := Json.tuple2 (,) Json.float Json.float)
    (Json.maybe ("foreign" := Json.string))
    (Json.maybe ("native" := Json.string))
