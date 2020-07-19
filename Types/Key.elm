module Types.Key exposing
    ( Key(..)
    , decoder
    )

import Json.Decode as Decode exposing (Decoder)


type Key
    = Character Char
    | Control String


decoder : Decoder Key
decoder =
    let
        toKey : String -> Key
        toKey string =
            case String.uncons string of
                Just ( char, "" ) ->
                    Character char

                _ ->
                    Control string
    in
    Decode.field "key" Decode.string
        |> Decode.map toKey
