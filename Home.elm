module Home exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attributes
import Json.Decode as Decode
import Types.Json as Json
import Types.Key as Key exposing (Key)
import Types.Return as Return
import Types.Tree as Tree


main : Program () Model Msg
main =
    { init = always ( initialModel, Cmd.none )
    , view = view
    , update = update
    , subscriptions =
        Key.decoder
            |> Decode.map KeyPressed
            |> Browser.Events.onKeyPress
            |> always
    }
        |> Browser.document


type Msg
    = KeyPressed Key


initialModel : Model
initialModel =
    { tree = Json.example
    , mode = Normal
    , keyString = []
    , lastKey = Nothing
    }


type alias Model =
    { tree : Json.Tree
    , mode : Mode
    , keyString : List Key
    , lastKey : Maybe Key
    }


type Mode
    = Normal
    | Insert


view : Model -> Browser.Document msg
view model =
    let
        modeClass =
            case model.mode of
                Normal ->
                    "normal-mode"

                Insert ->
                    "insert-mode"

        showKey key =
            case key of
                Key.Character c ->
                    String.fromChar c
                        |> Html.text

                Key.Control s ->
                    Html.text s

        reportKey key =
            Html.div
                [ Attributes.class "last-key-pressed" ]
                [ Html.text "Last key: "
                , showKey key
                ]

        currentCommand =
            Html.div
                [ Attributes.class "current-command" ]
                (Html.text "Current command: " :: List.map showKey model.keyString)
    in
    { title = "Treditor"
    , body =
        [ style
        , Html.header
            [ Attributes.class "header-info" ]
            [ Html.text "Mode: "
            , case model.mode of
                Normal ->
                    Html.text "Normal"

                Insert ->
                    Html.text "Insert"
            , Html.text " "
            , model.lastKey
                |> Maybe.map reportKey
                |> Maybe.withDefault (Html.text "")
            , currentCommand
            ]
        , Html.div
            [ Attributes.class modeClass ]
            [ Json.view model.tree ]
        ]
    }


style : Html msg
style =
    """
.normal-mode .cursor{
    border: 1px solid red;
}
.insert-mode .cursor{
    border: 1px solid darkgreen;
}
.string-literal, .boolean-literal, .number-literal{
    display: inline-block;

}
.object, .field, .list{
    list-style: none;
    padding-left: 4em;
    }
.field {
    display: flex;
    flex-direction: row;
    }
    """
        |> Html.text
        |> List.singleton
        |> Html.node "style" []


modifyTree : (Json.Tree -> Json.Tree) -> Model -> Model
modifyTree f model =
    { model | tree = f model.tree }


clearKeyString : Model -> Model
clearKeyString model =
    { model | keyString = [] }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        KeyPressed key ->
            { model
                | lastKey = Just key
                , keyString = List.append model.keyString [ key ]
            }
                |> interpretKeyString
                |> Return.noCommand


interpretKeyString : Model -> Model
interpretKeyString model =
    case model.mode of
        Normal ->
            case model.keyString of
                [ Key.Character 'i' ] ->
                    model
                        |> modifyTree Tree.goIn
                        |> clearKeyString

                [ Key.Character 'o' ] ->
                    model
                        |> modifyTree Tree.goOut
                        |> clearKeyString

                [ Key.Character 'k' ] ->
                    model
                        |> modifyTree Tree.goDown
                        |> clearKeyString

                [ Key.Character 'j' ] ->
                    model
                        |> modifyTree Tree.goUp
                        |> clearKeyString

                [ Key.Character 'e' ] ->
                    { model | mode = Insert }
                        |> clearKeyString

                [ Key.Character 'a' ] ->
                    model
                        |> modifyTree Json.addLocal
                        |> clearKeyString

                [ Key.Character 'c', Key.Character 'o' ] ->
                    model
                        |> modifyTree Json.changeToObject
                        |> clearKeyString

                _ ->
                    case List.member (Key.Character 'q') model.keyString of
                        True ->
                            model |> clearKeyString

                        False ->
                            model

        Insert ->
            case model.keyString of
                [ Key.Control "Enter" ] ->
                    { model | mode = Normal }
                        |> clearKeyString

                _ ->
                    List.foldl (Tree.insert >> modifyTree) model model.keyString
                        |> clearKeyString
