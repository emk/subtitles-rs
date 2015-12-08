module Util (listFromMaybe, checkbox, updateChild) where

import Effects
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

updateChild
  : action
  -> model
  -> (action -> model -> (model, Effects.Effects action))
  -> (action -> parentAction)
  -> (model -> parentModel)
  -> (parentModel, Effects.Effects parentAction)
updateChild msg model update tag assign =
  let
    (model', fx) = update msg model
    assigned = assign model'
  in (assigned, Effects.map tag fx)

