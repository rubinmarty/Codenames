module Vector exposing (..)


type alias Vector =
    ( Int, Int )


zero : Vector
zero =
    ( 0, 0 )


getX : Vector -> Int
getX =
    Tuple.first


getY : Vector -> Int
getY =
    Tuple.second


add : Vector -> Vector -> Vector
add v w =
    ( getX v + getX w, getY v + getY w )


invert : Vector -> Vector
invert v =
    ( negate <| getX v, negate <| getY v )


subtract : Vector -> Vector -> Vector
subtract v w =
    add v <| invert w


scale : Int -> Vector -> Vector
scale i v =
    ( getX v * i, getY v * i )


innerProduct : Vector -> Vector -> Int
innerProduct v w =
    (getX v * getX w + getY v * getY w)
