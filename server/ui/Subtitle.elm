module Subtitle (Model, init, view) where

import Html exposing (div, text, p)
import Html.Attributes exposing (class)

type alias Model =
  { period: (Float, Float)
  , foreignText: Maybe String
  , nativeText: Maybe String
  }

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
