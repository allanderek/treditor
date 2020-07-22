module Types.Json exposing
    ( Leaf(..)
    , Node(..)
    , Tree
    , addLocal
    , changeToObject
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


addLocal : Tree -> Tree
addLocal tree =
    case tree of
        Tree.Cursor subTree ->
            addLocal subTree
                |> Tree.Cursor

        Tree.Leaf _ _ ->
            tree

        Tree.Node kind subTrees ->
            let
                new =
                    case kind of
                        ObjectTree ->
                            Tree.Node FieldTree [ Tree.Leaf StringLiteral "", Tree.Leaf StringLiteral "" ]

                        FieldTree ->
                            -- This shouldn't really happen you should always have exactly two
                            Tree.Leaf StringLiteral ""

                        ListTree ->
                            Tree.Leaf StringLiteral ""

                add l =
                    case l of
                        [] ->
                            []

                        (Tree.Cursor t) :: rest ->
                            Tree.Cursor t :: new :: add rest

                        first :: rest ->
                            first :: add rest
            in
            subTrees
                |> List.map addLocal
                |> add
                |> Tree.Node kind


changeToObject : Tree -> Tree
changeToObject tree =
    case tree of
        Tree.Cursor subTree ->
            -- We can be more clever about this, for example if it is already an object leave it,
            -- if it is a list use the list elements as valeus for fields with empty names
            [ Tree.Node FieldTree [ Tree.Leaf StringLiteral "", Tree.Leaf StringLiteral "" ] ]
                |> Tree.Node ObjectTree
                |> Tree.Cursor

        Tree.Leaf _ _ ->
            tree

        Tree.Node kind subTrees ->
            subTrees
                |> List.map changeToObject
                |> Tree.Node kind
