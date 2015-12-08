module Util
  (listFromMaybe, listFromMaybes, checkbox, updateChild, maybeUpdateChild) where

import Effects
import Html exposing (input)
import Html.Attributes exposing (class, type', checked)
import Html.Events exposing (on, targetChecked)

listFromMaybe : Maybe a -> List a
listFromMaybe maybeVal =
  case maybeVal of
    Just val -> [val]
    Nothing -> []

listFromMaybes : List (Maybe a) -> List a
listFromMaybes l =
  case l of
    [] -> []
    Nothing :: xs -> listFromMaybes xs
    Just x :: xs -> x :: listFromMaybes xs

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

maybeUpdateChild
  : action
  -> (Maybe model)
  -> (action -> model -> (model, Effects.Effects action))
  -> (action -> parentAction)
  -> parentModel
  -> (model -> parentModel)
  -> (parentModel, Effects.Effects parentAction)
maybeUpdateChild msg maybeModel update tag parentModel assign =
  case maybeModel of
    Just model ->
      updateChild msg model update tag assign
    -- This is probably an error that we should log:
    Nothing ->
      (parentModel, Effects.none)
