module Grid exposing (Grid, grid, lookup, lookupV, set, setV, allVectors, render, map, indexedMap, mapAtV)

import Vector exposing (..)
import List exposing (drop, head, take, repeat, concatMap, repeat, range)
import Maybe exposing (Maybe, andThen, withDefault)
import Basics exposing (uncurry)
import Html
import Html.Attributes

type alias Grid a = List (List a)

grid : Int -> Int -> a -> Grid a
grid x y v =
    repeat y <| repeat x v

height : Grid a -> Int
height grid =
    List.length grid

length : Grid a -> Int
length grid =
    List.length <| withDefault [] <| head grid 

lookup : Int -> Int -> Grid a -> Maybe a
lookup x y grid =
    grid
    |> drop y
    |> head
    |> andThen (Just << drop x)
    |> andThen head

lookupV : Vector -> Grid a -> Maybe a
lookupV = uncurry lookup
 
set : Int -> Int -> a -> Grid a -> Grid a
set x y val grid =
    let set_ x_ val_ m_list_ =
        case m_list_ of
            Nothing -> []
            Just list_ ->
                [
                    take x_ list_ ++
                    [val_] ++
                    drop (x_ + 1) list_
                ]
    in

    take y grid ++
    (set_ x val <| head <| drop y grid) ++
    drop (y + 1) grid

setV : Vector -> a -> Grid a -> Grid a
setV = uncurry set

allVectors : Grid a -> List Vector
allVectors grid =
    let
        x = length grid
        y = height grid
    in
        range 0 x
        |> List.concatMap (\x -> range 0 y
            |> List.map (\y -> (x,y))
        )

map : (a -> b) -> Grid a -> Grid b
map f grid =
    List.map (List.map f) grid

mapAtV : (a -> a) -> Vector -> Grid a -> Grid a 
mapAtV f v grid =
    indexedMap (\v2 elt -> if v==v2 then f elt else elt) grid

indexedMap : (Vector -> a -> b) -> Grid a -> Grid b
indexedMap f grid =
    List.indexedMap (\y -> List.indexedMap ((flip <| curry f) y)) grid


render : (Vector -> a -> Html.Html b) -> Grid a -> Html.Html b
render f grid =
    indexedMap f grid
    |> List.map (Html.div [])
    |> Html.div [Html.Attributes.style [("font-size", "0px")]]
