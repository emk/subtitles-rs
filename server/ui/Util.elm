module Util (listFromMaybe, checkbox) where

import Html exposing (input)
import Html.Attributes exposing (class, type', checked)
import Html.Events exposing (on, targetChecked)

listFromMaybe : Maybe a -> List a
listFromMaybe maybeVal =
  case maybeVal of
    Just val -> [val]
    Nothing -> []

checkbox : Signal.Address a -> Bool -> (Bool -> a) -> Html.Html
checkbox address isChecked tag =
  input
    [ type' "checkbox"
    , checked isChecked
    , on "change" targetChecked (tag >> Signal.message address)
    ]
    []
