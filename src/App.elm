module App exposing (app)

import Types exposing (..)
import State exposing (..)
import View exposing (view)
import Grid exposing (Grid, lookupV, setV)
import Vector exposing (Vector, getX, getY)

import Html exposing (Html, Attribute, div, button, text, span)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Maybe exposing (andThen, withDefault)
import Random
import RandomList exposing (shuffle, get)
import WordLists
import WebSocket

app =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }