import Effects exposing (Never)
import StartApp
import Task

import Html

import Hello

-- Define our application using the StartApp MVC library to handle our
-- basic architecture.
app : StartApp.App Hello.Model
app =
  StartApp.start
    { init = Hello.init
    , view = Hello.view
    , update = Hello.update
    , inputs = [] -- for receiving messages from JavaScript
    }

-- Display the HTML of our application.
main : Signal Html.Html
main = app.html

-- This is apparently how our runnable tasks get connected to JavaScript.
port tasks : Signal (Task.Task Never ())
port tasks = app.tasks
