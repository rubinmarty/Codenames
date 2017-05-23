module Grid exposing (..)

import Array exposing (Array)
import Array.Extra
import Maybe exposing (Maybe)
import Vector exposing (Vector)


type alias Grid a =
    Array (Array a)


type alias GridFold a b c =
    Combine a b -> Combine b c -> b -> c -> Grid a -> c


type alias ArrayFold a b =
    Combine a b -> b -> Array a -> b


type alias Combine a b =
    a -> b -> b


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
        |> Maybe.map Array.length
        |> Maybe.withDefault 0


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
countIf predicate grid =
    let
        incrIf x acc =
            if predicate x then
                1 + acc
            else
                acc
    in
        foldLeftTop incrIf (+) 0 0 grid


genericFold : ArrayFold a b -> ArrayFold (Array a) c -> GridFold a b c
genericFold foldh foldv combine combineRows base baseRow grid =
    let
        foldRow : Array a -> b
        foldRow =
            foldh combine base

        foldAndCombineRow : Combine (Array a) c
        foldAndCombineRow row acc =
            combineRows (foldRow row) acc
    in
        foldv foldAndCombineRow baseRow grid


foldLeftTop : GridFold a b c
foldLeftTop =
    genericFold Array.foldl Array.foldl


foldLeftBottom : GridFold a b c
foldLeftBottom =
    genericFold Array.foldl Array.foldr


foldRightBottom : GridFold a b c
foldRightBottom =
    genericFold Array.foldr Array.foldr


foldRightTop : GridFold a b c
foldRightTop =
    genericFold Array.foldr Array.foldl


filter : (a -> Bool) -> Grid a -> Grid (Maybe a)
filter predicate grid =
    map
        (\x ->
            if predicate x then
                Just x
            else
                Nothing
        )
        grid
