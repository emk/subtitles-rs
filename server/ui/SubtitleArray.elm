module SubtitleArray (Model, Action, update, viewAt, timeToIndex) where

import Array
import Html

import Subtitle exposing (startTime, endTime)

type alias Model = Array.Array Subtitle.Model

type Action = ItemN Int Subtitle.Action

update : Action -> Model -> Array.Array Subtitle.Model
update action model =
  case action of
    ItemN idx act ->
      case Array.get idx model of
        Nothing -> model
        Just sub -> Array.set idx (Subtitle.update act sub) model

viewAt : Int -> Signal.Address Action -> Model -> Maybe Html.Html
viewAt idx address model =
  case Array.get idx model of
    Nothing -> Nothing
    Just model' ->
      Just <| Subtitle.view (Signal.forwardTo address (ItemN idx)) model'

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
