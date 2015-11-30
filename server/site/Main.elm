import Effects exposing (Never)
import Html exposing (text)
import Http
import Json.Decode as Json
import StartApp
import Task
import Time exposing (..)

-- Decode a simple message from our server.
decodeMessage = Json.at ["message"] Json.string

-- The URL of our server-side API.
messageUrl = Http.url "http://localhost:8080/api/v1/hello" []

-- Fetch a message from our server.
getMessage =
  Http.get decodeMessage messageUrl
    |> Task.toMaybe
    |> Task.map NewMessage
    |> Effects.task

type alias Model = { message: String }

type Action = NewMessage (Maybe String)

init = (Model "Loading...", getMessage)

update msg model =
  case msg of
    NewMessage Nothing ->
      ({ model | message = "Error contacting server" }, Effects.none)
    NewMessage (Just message) ->
      ({ model | message = message }, Effects.none)

view address model = text model.message

-- Define our application using the StartApp MVC library to handle our
-- basic architecture.
app =
  StartApp.start
    { init = init
    , view = view
    , update = update
    , inputs = [] -- for receiving messages from JavaScript
    }

-- Display the HTML of our application.
main = app.html

-- This is apparently how our runnable tasks get connected to JavaScript.
port tasks : Signal (Task.Task Never ())
port tasks = app.tasks
