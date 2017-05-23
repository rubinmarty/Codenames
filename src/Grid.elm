module Grid exposing (..)

import Array exposing (Array)
import Array.Extra
import Basics exposing (uncurry)
import Maybe exposing (Maybe)
import Vector exposing (Vector)


type alias Grid a =
    Array (Array a)


grid : Int -> Int -> a -> Grid a
grid x y v =
    v
        |> Array.repeat x
        |> Array.repeat y


height : Grid a -> Int
height grid =
    Array.length grid


width : Grid a -> Int
width grid =
    grid
        |> Array.get 0
        |> Maybe.withDefault Array.empty
        |> Array.length


dimensions : Grid a -> Vector
dimensions grid =
    ( width grid, height grid )


lookup : Int -> Int -> Grid a -> Maybe a
lookup x y grid =
    grid
        |> Array.get y
        |> Maybe.andThen (Array.get x)


lookupV : Vector -> Grid a -> Maybe a
lookupV =
    uncurry lookup


set : Int -> Int -> a -> Grid a -> Grid a
set x y val grid =
    Array.Extra.update y (Array.set x val) grid


setV : Vector -> a -> Grid a -> Grid a
setV =
    uncurry set


update : Vector -> (a -> a) -> Grid a -> Grid a
update ( x, y ) f grid =
    Array.Extra.update y (Array.Extra.update x f) grid


map : (a -> b) -> Grid a -> Grid b
map f grid =
    Array.map (Array.map f) grid


indexedMap : (Vector -> a -> b) -> Grid a -> Grid b
indexedMap f grid =
    Array.indexedMap (\y -> Array.indexedMap ((flip <| curry f) y)) grid


countIf : (a -> Bool) -> Grid a -> Int
countIf pred grid =
    foldLeftTop
        (\x acc ->
            if pred x then
                1 + acc
            else
                acc
        )
        (\x acc -> x + acc)
        0
        0
        grid


foldLeftTop : (a -> b -> b) -> (b -> c -> c) -> b -> c -> Grid a -> c
foldLeftTop combine combineRows base baseRow grid =
    let
        foldRow row =
            Array.foldl combine base row

        foldAndCombineRow row acc =
            combineRows (foldRow row) acc
    in
        Array.foldl foldAndCombineRow baseRow grid


foldRightBottom : (a -> b -> b) -> (b -> c -> c) -> b -> c -> Grid a -> c
foldRightBottom combine combineRows base baseRow grid =
    let
        foldRow row =
            Array.foldr combine base row

        foldAndCombineRow row acc =
            combineRows (foldRow row) acc
    in
        Array.foldr foldAndCombineRow baseRow grid
