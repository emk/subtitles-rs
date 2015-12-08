module Subtitle.Array
  (Model, Action, update, viewAt, viewsAt, timeToIndex) where

import Array
import Effects
import Html
import Signal exposing (Address)

import Subtitle exposing (startTime, endTime)
import Util exposing (listFromMaybes, maybeUpdateChild)
import VideoPlayer

type alias Model = Array.Array Subtitle.Model

type Action = ItemN Int Subtitle.Action

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    ItemN idx act ->
      maybeUpdateChild act (Array.get idx model) Subtitle.update (ItemN idx)
        model (\s -> Array.set idx s model)

viewAt : Int -> Address VideoPlayer.Action -> Address Action -> Model
  -> Maybe Html.Html
viewAt idx playerAddress address model =
  let addr = Signal.forwardTo address (ItemN idx)
  in Maybe.map (\m -> Subtitle.view playerAddress addr m) (Array.get idx model)

viewsAt : List Int -> Address VideoPlayer.Action -> Address Action -> Model
  -> List Html.Html
viewsAt indices playerAddress address model =
  indices
    |> List.map (\idx -> viewAt idx playerAddress address model)
    |> listFromMaybes

type TimeRelation = Before | During | After

timeRelation : Float -> Subtitle.Model -> TimeRelation
timeRelation time subtitle =
  if time < (startTime subtitle) then
    Before
  else if time > (endTime subtitle) then
    After
  else
    During

timeToIndexHelper : Float -> Model -> Int -> Int
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

timeToIndex : Float -> Model -> Int
timeToIndex time subtitles =
  timeToIndexHelper time subtitles 0
