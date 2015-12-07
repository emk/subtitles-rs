module Subtitle (Model, init, Action, update, view, decode, timeToIndex) where

import Array
import Html exposing (div, text, p, input)
import Html.Attributes exposing (class, type', checked)
import Html.Events exposing (on, targetChecked)
import Json.Decode as Json exposing ((:=))
import Signal

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

update : Action -> Model -> Model
update action model =
  case action of
    Selected val -> { model | selected = val }

view : Signal.Address Action -> Model -> Html.Html
view address model =
  let
    check =
      input
        [ type' "checkbox"
        , checked (model.selected)
        , on "change" targetChecked (Signal.message address << Selected)
        ]
        []
    foreignHtml =
      case model.foreignText of
        Just t -> [p [class "foreign"] [text t]]
        Nothing -> []
    nativeHtml =
      case model.nativeText of
        Just t -> [p [class "native"] [text t]]
        Nothing -> []
  in div [class "subtitle"] ([check] ++ foreignHtml ++ nativeHtml)

decode : Json.Decoder Model
decode =
  Json.object3 init
    ("period" := Json.tuple2 (,) Json.float Json.float)
    (Json.maybe ("foreign" := Json.string))
    (Json.maybe ("native" := Json.string))

type TimeRelation = Before | During | After

timeRelation : Float -> Model -> TimeRelation
timeRelation time subtitle =
  if time < (startTime subtitle) then
    Before
  else if time > (endTime subtitle) then
    After
  else
    During

timeToIndexHelper : Float -> Array.Array Model -> Int -> Int
timeToIndexHelper time subtitles idx =
  case Array.get idx subtitles of
    -- We're beyond the end of our array, so return the current index.
    Nothing -> idx
    Just sub ->
      case timeRelation time sub of
        -- Keep looking if we're after the current sub.
        After -> timeToIndexHelper time subtitles (idx + 1)
        -- We're either before or in this sub, so we found it.
        _ -> idx

timeToIndex : Float -> Array.Array Model -> Int
timeToIndex time subtitles =
  timeToIndexHelper time subtitles 0
