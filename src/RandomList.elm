module RandomList exposing (..)

import Random exposing (..)

constant : a -> Generator a
constant value =
    Random.map (\_ -> value) Random.bool

get : Int -> List a -> Maybe a
get index list =
    list
        |> List.drop index
        |> List.head

choose : List a -> Generator ( Maybe a, List a )
choose list =
    if List.isEmpty list then
        constant ( Nothing, list )
    else
        let
            lastIndex =
                List.length list - 1

            front i =
                List.take i list

            back i =
                List.drop (i + 1) list

            gen =
                Random.int 0 lastIndex
        in
            Random.map
                (\index ->
                    ( get index list, List.append (front index) (back index) )
                )
                gen

shuffle : List a -> Generator (List a)
shuffle list =
    if List.isEmpty list then
        constant list
    else
        let
            helper : ( List a, List a ) -> Generator ( List a, List a )
            helper ( done, remaining ) =
                choose remaining
                    |> Random.andThen
                        (\( m_val, shorter ) ->
                            case m_val of
                                Nothing ->
                                    constant ( done, shorter )

                                Just val ->
                                    helper ( val :: done, shorter )
                        )
        in
            Random.map Tuple.first (helper ( [], list ))