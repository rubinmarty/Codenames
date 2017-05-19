module App exposing (app)

import Navigation
import State exposing (init, update, subscriptions)
import View exposing (view)
import Types exposing (Msg(UrlChange))


app =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
