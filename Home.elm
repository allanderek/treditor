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
    , lastKey = Nothing
    }


type alias Model =
    { tree : Json.Tree
    , mode : Mode
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

        reportKey key =
            Html.div
                [ Attributes.class "last-key-pressed" ]
                [ Html.text "Last key: "
                , case key of
                    Key.Character c ->
                        String.fromChar c
                            |> Html.text

                    Key.Control s ->
                        Html.text s
                ]
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


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        KeyPressed key ->
            { model | lastKey = Just key }
                |> interpretKey key
                |> Return.noCommand


interpretKey : Key -> Model -> Model
interpretKey key model =
    case model.mode of
        Normal ->
            case key of
                Key.Character 'i' ->
                    model
                        |> modifyTree Tree.goIn

                Key.Character 'o' ->
                    model
                        |> modifyTree Tree.goOut

                Key.Character 'k' ->
                    model
                        |> modifyTree Tree.goDown

                Key.Character 'j' ->
                    model
                        |> modifyTree Tree.goUp

                Key.Character 'e' ->
                    { model | mode = Insert }

                _ ->
                    model

        Insert ->
            case key of
                Key.Control "Enter" ->
                    { model | mode = Normal }

                _ ->
                    model
                        |> modifyTree (Tree.insert key)
