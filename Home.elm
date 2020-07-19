module Home exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attributes
import Json.Decode as Decode exposing (Decoder)
import Return


type Key
    = Character Char
    | Control String


keyDecoder : Decoder Msg
keyDecoder =
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
        |> Decode.map (toKey >> KeyPressed)


main : Program () Model Msg
main =
    { init = always ( initialTree, Cmd.none )
    , view = view
    , update = update
    , subscriptions =
        Browser.Events.onKeyPress keyDecoder
            |> always
    }
        |> Browser.document


initialTree : JsonTree
initialTree =
    Node ObjectTree
        [ Node FieldTree [ Leaf StringLiteral "name", Leaf StringLiteral "allan" ]
        , Node FieldTree [ Leaf StringLiteral "age", Leaf NumberLiteral "39" ]
        ]
        |> Cursor


type Msg
    = KeyPressed Key


type alias Model =
    JsonTree


type JsonTree
    = Cursor JsonTree
    | Leaf LeafKind String
    | Node NodeKind (List JsonTree)


type LeafKind
    = StringLiteral
    | BooleanLiteral
    | NumberLiteral


type NodeKind
    = ObjectTree
    | FieldTree
    | ListTree


type alias Field =
    { name : String
    , value : JsonTree
    }


view : Model -> Browser.Document msg
view model =
    { title = "Treditor"
    , body =
        [ style
        , viewJsonTree model
        ]
    }


viewJsonTree : JsonTree -> Html msg
viewJsonTree tree =
    let
        simpleList listClass elementClass separator elements =
            List.map List.singleton elements
                |> List.map (Html.li [ Attributes.class elementClass ])
                |> List.intersperse (punctuation separator)
                |> Html.ul [ Attributes.class listClass ]

        punctuation s =
            Html.span [ Attributes.class "punctuation" ] [ Html.text s ]

        surround left right element =
            Html.div
                [ Attributes.class "surround" ]
                [ Html.span [ Attributes.class "surround" ] [ Html.text left ]
                , element
                , Html.span [ Attributes.class "surround" ] [ Html.text right ]
                ]
    in
    case tree of
        Cursor subTree ->
            Html.div
                [ Attributes.class "cursor" ]
                [ viewJsonTree subTree ]

        Leaf StringLiteral s ->
            Html.div
                [ Attributes.class "string-literal" ]
                [ Html.text "\""
                , Html.text s
                , Html.text "\""
                ]

        Leaf BooleanLiteral s ->
            Html.div
                [ Attributes.class "boolean-literal" ]
                [ s |> Html.text ]

        Leaf NumberLiteral s ->
            Html.div
                [ Attributes.class "number-literal" ]
                [ Html.text s ]

        Node kind fields ->
            let
                subFields =
                    List.map viewJsonTree fields
            in
            case kind of
                ObjectTree ->
                    subFields
                        |> simpleList "object" "object-field" ","
                        |> surround "{" "}"

                FieldTree ->
                    subFields
                        |> simpleList "field" "field-part" ":"

                ListTree ->
                    subFields
                        |> simpleList "list" "list-element" ","
                        |> surround "[" "]"


style : Html msg
style =
    """
.cursor{
    border: 1px solid red;
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


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        KeyPressed key ->
            case key of
                Character 'i' ->
                    goIn model
                        |> Return.noCommand

                Character 'o' ->
                    goOut model
                        |> Return.noCommand

                _ ->
                    Return.noCommand model


goIn : JsonTree -> JsonTree
goIn tree =
    case tree of
        Cursor (Node kind (firstChild :: others)) ->
            Node kind (Cursor firstChild :: others)

        Cursor _ ->
            tree

        Leaf _ _ ->
            tree

        Node kind subTrees ->
            List.map goIn subTrees
                |> Node kind


isCursor : JsonTree -> Bool
isCursor tree =
    case tree of
        Cursor _ ->
            True

        Node _ _ ->
            False

        Leaf _ _ ->
            False


removeCursor : JsonTree -> JsonTree
removeCursor tree =
    -- Note, this only removes it from the top level if it is there, it does not recursively remove any
    -- cursors contained within the tree.
    case tree of
        Cursor subTree ->
            subTree

        Node _ _ ->
            tree

        Leaf _ _ ->
            tree


goOut : JsonTree -> JsonTree
goOut tree =
    case tree of
        Node kind subTrees ->
            let
                newTree =
                    List.map removeCursor subTrees
                        |> List.map goOut
                        |> Node kind
            in
            -- Note: obviously we are checking the *old* subTrees, the new subTrees
            -- may have cursor that has already been moved out.
            case List.any isCursor subTrees of
                True ->
                    Cursor newTree

                False ->
                    newTree

        Cursor _ ->
            -- Why not remove it? We remove it explicitly in the case above, here it could be the outermost
            -- tree so we don't wish to remove it from there.
            tree

        Leaf _ _ ->
            tree
