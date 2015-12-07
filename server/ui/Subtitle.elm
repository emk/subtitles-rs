module Subtitle (Model, init, view, decode, timeToIndex) where

import Array
import Html exposing (div, text, p)
import Html.Attributes exposing (class)
import Json.Decode as Json exposing ((:=))

type alias Model =
  { period: (Float, Float)
  , foreignText: Maybe String
  , nativeText: Maybe String
  }

startTime : Model -> Float
startTime model = fst model.period

endTime : Model -> Float
endTime model = snd model.period

init : (Float, Float) -> Maybe String -> Maybe String -> Model
init = Model

view : Model -> Html.Html
view model =
  let
    foreignHtml =
      case model.foreignText of
        Just t -> [p [class "foreign"] [text t]]
        Nothing -> []
    nativeHtml =
      case model.nativeText of
        Just t -> [p [class "native"] [text t]]
        Nothing -> []
  in div [class "subtitle"] (foreignHtml ++ nativeHtml)

decode : Json.Decoder Model
decode =
  Json.object3 Model
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
