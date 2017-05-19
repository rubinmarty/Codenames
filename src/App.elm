module App exposing (app)

import State exposing (..)
import View exposing (view)

import Html

app =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }