module Types.Json exposing
    ( Leaf(..)
    , Node(..)
    , Tree
    , example
    , view
    )

import Html exposing (Html)
import Html.Attributes as Attributes
import Types.Tree as Tree


type alias Tree =
    Tree.Tree Node Leaf


type Leaf
    = StringLiteral
    | BooleanLiteral
    | NumberLiteral


type Node
    = ObjectTree
    | FieldTree
    | ListTree


example : Tree
example =
    Tree.Node ObjectTree
        [ Tree.Node FieldTree [ Tree.Leaf StringLiteral "name", Tree.Leaf StringLiteral "allan" ]
        , Tree.Node FieldTree [ Tree.Leaf StringLiteral "age", Tree.Leaf NumberLiteral "39" ]
        ]
        |> Tree.Cursor


view : Tree -> Html msg
view tree =
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
        Tree.Cursor subTree ->
            Html.div
                [ Attributes.class "cursor" ]
                [ view subTree ]

        Tree.Leaf StringLiteral s ->
            Html.div
                [ Attributes.class "string-literal" ]
                [ Html.text "\""
                , Html.text s
                , Html.text "\""
                ]

        Tree.Leaf BooleanLiteral s ->
            Html.div
                [ Attributes.class "boolean-literal" ]
                [ s |> Html.text ]

        Tree.Leaf NumberLiteral s ->
            Html.div
                [ Attributes.class "number-literal" ]
                [ Html.text s ]

        Tree.Node kind fields ->
            let
                subFields =
                    List.map view fields
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
