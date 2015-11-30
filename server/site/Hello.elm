module Hello (Model, Action, init, update, view) where

import Effects exposing (Never)
import Html exposing (text)
import Http
import Json.Decode as Json
import Task

-- The data we're working with.
type alias Model = { message: String }

-- Available actions which we can perform.
type Action = NewMessage (Maybe String)

-- Create our initial model.
init : (Model, Effects.Effects Action)
init = (Model "Loading...", getMessage)

-- Update our model according to a message.
update : Action -> Model -> (Model, Effects.Effects Action)
update msg model =
  case msg of
    NewMessage Nothing ->
      ({ model | message = "Error contacting server" }, Effects.none)
    NewMessage (Just message) ->
      ({ model | message = message }, Effects.none)

-- Render our model as HTML, sending any actions to the specified address.
view : Signal.Address Action -> Model -> Html.Html
view address model = text model.message


---------------------------------------------------------------------------
-- Helpers

-- Decode a simple JSON message from our server.
decodeMessage : Json.Decoder String
decodeMessage = Json.at ["message"] Json.string

-- The URL of our server-side API.
messageUrl : String
messageUrl = Http.url "/api/v1/hello" []

-- Fetch a message from our server.
getMessage : Effects.Effects Action
getMessage =
  Http.get decodeMessage messageUrl
    |> Task.toMaybe
    |> Task.map NewMessage
    |> Effects.task
