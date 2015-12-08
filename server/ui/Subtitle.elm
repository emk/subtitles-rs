module Subtitle
  (Model, startTime, endTime, init, Action, update, view, decode) where

import Effects
import Html exposing (div, text, p, input)
import Html.Attributes exposing (class, type', checked)
import Json.Decode as Json exposing ((:=))
import Signal

import Util exposing (listFromMaybes, checkbox)

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

view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    check = checkbox address model.selected Selected
    foreignHtml =
      Maybe.map (\t -> p [class "foreign"] [text t]) model.foreignText
    nativeHtml =
      Maybe.map (\t -> p [class "native"] [text t]) model.nativeText
    children = 
      [check] ++ listFromMaybes [foreignHtml, nativeHtml]
  in div [class "subtitle"] children

decode : Json.Decoder Model
decode =
  Json.object3 init
    ("period" := Json.tuple2 (,) Json.float Json.float)
    (Json.maybe ("foreign" := Json.string))
    (Json.maybe ("native" := Json.string))
