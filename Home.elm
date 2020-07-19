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
    }


type alias Model =
    { tree : Json.Tree
    , mode : Mode
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
    in
    { title = "Treditor"
    , body =
        [ style
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
            case model.mode of
                Normal ->
                    case key of
                        Key.Character 'i' ->
                            model
                                |> modifyTree Tree.goIn
                                |> Return.noCommand

                        Key.Character 'o' ->
                            model
                                |> modifyTree Tree.goOut
                                |> Return.noCommand

                        Key.Character 'e' ->
                            { model | mode = Insert }
                                |> Return.noCommand

                        _ ->
                            Return.noCommand model

                Insert ->
                    model
                        |> modifyTree (Tree.insert key)
                        |> Return.noCommand
